use inner::{NewObject, Pointer, Queue};
use piccolo::runtime::value::Value;

#[derive(Debug)]
struct Cons {
    car: i64,
    cdr: Pointer,
}

impl NewObject for Cons {
    fn trace(&self, queue: &Queue) {
        queue.trace(self.cdr);
    }
}

fn main() {
    println!("not rooted");
    run(false);

    println!("rooted");
    run(true);
}

fn run(root: bool) {
    use inner::HeapThread;

    let ht = HeapThread::new();
    let queue = ht.queue();

    let ptr = queue.allocate_rooted(Box::new(Cons {
        car: 0,
        cdr: queue.allocate_temporary(Box::new(Cons {
            car: 4,
            cdr: Pointer::null(),
        })),
    }));

    let do_get: inner::Get = |_, object, property| -> Value {
        assert!(property.is_nil());
        Value::Integer(object.downcast_ref::<Cons>().unwrap().car)
    };
    let inner = queue.get(ptr, Value::Nil, do_get);
    assert!(matches!(inner, Value::Integer(0)));

    if root {
        let do_mutate: inner::Mutate = |queue, object, property, new_value| {
            assert!(property.is_nil());
            let new_value = new_value.as_integer();
            object.downcast_mut::<Cons>().unwrap().car = new_value;
            queue.root(object.downcast_mut::<Cons>().unwrap().cdr);
        };
        queue.mutate(ptr, Value::Nil, Value::Integer(5), do_mutate);
    } else {
        let do_mutate: inner::Mutate = |_, object, property, new_value| {
            assert!(property.is_nil());
            let new_value = new_value.as_integer();
            object.downcast_mut::<Cons>().unwrap().car = new_value;
        };
        queue.mutate(ptr, Value::Nil, Value::Integer(5), do_mutate);
    }

    let inner = queue.get(ptr, Value::Nil, do_get);
    assert!(matches!(inner, Value::Integer(5)));

    queue.debug();
    queue.collect();
    queue.debug();
    ht.join();
}

mod inner {
    use piccolo::runtime::value::Value;
    use slotmap::{DefaultKey, Key, SlotMap};
    use std::{
        sync::{
            atomic::AtomicUsize,
            mpsc::{self, Receiver, Sender},
        },
        thread::JoinHandle,
    };

    pub trait NewObject: downcast_rs::Downcast + std::fmt::Debug + Send {
        fn trace(&self, queue: &Queue);
    }

    downcast_rs::impl_downcast!(NewObject);

    #[derive(Debug, Clone, Copy)]
    pub struct Pointer(DefaultKey);

    impl Pointer {
        pub fn null() -> Self {
            Pointer(DefaultKey::null())
        }
    }

    #[derive(Debug)]
    struct ObjectHeader {
        inner: Box<dyn NewObject>,
        rooted: bool,
        marked: bool,
    }

    pub type Mutate = fn(Queue, &mut dyn NewObject, Value, Value);
    pub type Get = fn(Queue, &dyn NewObject, Value) -> Value;

    enum Message {
        Allocate {
            who: Sender<Pointer>,
            obj: Box<dyn NewObject>,
            rooted: bool,
        },
        Mutate {
            who: Sender<()>,
            queue: Queue,
            ptr: Pointer,
            property: Value,
            new_value: Value,
            f: Mutate,
        },
        Get {
            who: Sender<Value>,
            queue: Queue,
            ptr: Pointer,
            property: Value,
            f: Get,
        },
        Collect {
            queue: Queue,
        },
        Trace {
            queue: Queue,
            ptr: Pointer,
        },
        Root {
            ptr: Pointer,
        },
        Debug,
        Done,
    }

    static COUNT: AtomicUsize = AtomicUsize::new(0);

    pub struct Queue {
        sender: Sender<Message>,
    }

    impl Clone for Queue {
        fn clone(&self) -> Self {
            COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            Queue {
                sender: self.sender.clone(),
            }
        }
    }

    impl Drop for Queue {
        fn drop(&mut self) {
            COUNT.fetch_sub(1, std::sync::atomic::Ordering::Relaxed);
        }
    }

    impl Queue {
        pub fn allocate_rooted(&self, obj: Box<dyn NewObject>) -> Pointer {
            self.allocate(obj, true)
        }

        pub fn allocate_temporary(&self, obj: Box<dyn NewObject>) -> Pointer {
            self.allocate(obj, false)
        }

        pub fn root(&self, ptr: Pointer) {
            self.sender.send(Message::Root { ptr }).unwrap();
        }

        pub fn allocate(&self, obj: Box<dyn NewObject>, rooted: bool) -> Pointer {
            let (this, result) = mpsc::channel();
            self.sender
                .send(Message::Allocate {
                    who: this,
                    obj,
                    rooted,
                })
                .unwrap();
            result.recv().unwrap()
        }

        pub fn get(&self, ptr: Pointer, property: Value, f: Get) -> Value {
            let (this, result) = mpsc::channel();
            self.sender
                .send(Message::Get {
                    who: this,
                    queue: self.clone(),
                    ptr,
                    property,
                    f,
                })
                .unwrap();
            result.recv().unwrap()
        }

        pub fn mutate(&self, ptr: Pointer, property: Value, new_value: Value, f: Mutate) {
            let (this, result) = mpsc::channel();
            self.sender
                .send(Message::Mutate {
                    who: this,
                    queue: self.clone(),
                    ptr,
                    property,
                    new_value,
                    f,
                })
                .unwrap();
            result.recv().unwrap()
        }

        pub fn trace(&self, ptr: Pointer) {
            self.sender
                .send(Message::Trace {
                    queue: self.clone(),
                    ptr,
                })
                .unwrap();
        }

        pub fn collect(&self) {
            self.sender
                .send(Message::Collect {
                    queue: self.clone(),
                })
                .unwrap();
        }

        pub fn debug(&self) {
            self.sender.send(Message::Debug).unwrap();
        }
    }

    pub struct HeapThread {
        _handle: JoinHandle<()>,
        sender: Sender<Message>,
    }

    impl HeapThread {
        pub fn new() -> HeapThread {
            let (sender, queue) = mpsc::channel();
            let _handle = std::thread::spawn(|| handle(queue));
            HeapThread { _handle, sender }
        }

        pub fn queue(&self) -> Queue {
            Queue {
                sender: self.sender.clone(),
            }
        }

        pub fn join(self) {
            self.sender.send(Message::Done).unwrap();
            drop(self.sender);
            self._handle.join().unwrap();
        }
    }

    fn handle(msg_queue: Receiver<Message>) {
        let mut heap = SlotMap::new();

        while let Ok(msg) = msg_queue.recv() {
            match msg {
                Message::Allocate { who, obj, rooted } => {
                    let key = heap.insert(ObjectHeader {
                        inner: obj,
                        marked: false,
                        rooted,
                    });

                    who.send(Pointer(key)).unwrap();
                }

                Message::Get {
                    who,
                    queue,
                    ptr,
                    property,
                    f,
                } => {
                    who.send(f(queue, heap[ptr.0].inner.as_ref(), property))
                        .unwrap();
                }

                Message::Mutate {
                    who,
                    queue,
                    ptr,
                    property,
                    new_value,
                    f,
                } => {
                    f(queue, heap[ptr.0].inner.as_mut(), property, new_value);
                    who.send(()).unwrap();
                }

                Message::Collect { queue } => {
                    for obj in heap.values_mut() {
                        obj.marked = false;
                    }
                    for obj in heap.values_mut() {
                        if obj.rooted {
                            obj.marked = true;
                            obj.inner.trace(&queue);
                        }
                    }
                    heap.retain(|_, obj| obj.marked);
                }

                Message::Trace { queue, ptr } => {
                    if let Some(obj) = heap.get_mut(ptr.0) {
                        if obj.rooted {
                            obj.marked = true;
                            obj.inner.trace(&queue);
                        }
                    }
                }

                Message::Root { ptr } => {
                    heap[ptr.0].rooted = true;
                }

                Message::Debug => {
                    println!(
                        "{} queues",
                        COUNT.load(std::sync::atomic::Ordering::Relaxed)
                    );
                    println!("{heap:#?}");
                }

                Message::Done => break,
            }
        }
    }
}
