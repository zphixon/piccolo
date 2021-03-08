//! Modules for the runtime representation and interpretation of Piccolo bytecode.

pub mod chunk;
pub mod memory;
pub mod object;
pub mod op;
pub mod value;
pub mod vm;

pub type ConstantIdx = u16;
pub type LocalSlotIdx = u16;
pub type LocalScopeDepth = u16;
pub type Line = usize;
pub type ChunkOffset = usize;
pub type HeapPtr = usize;
pub type StringPtr = usize;

#[test]
fn ptr_funnies() {
    use crate::{Object, Value};

    #[derive(PartialEq, Debug)]
    struct Upvalue {
        pub data: Value,
    }
    impl Object for Upvalue {
        fn type_name(&self) -> &'static str {
            "upvalue"
        }
    }
    impl core::fmt::Display for Upvalue {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{:?}", self.data)
        }
    }

    #[derive(Debug, PartialEq)]
    struct S(i64);
    impl Object for S {
        fn type_name(&self) -> &'static str {
            "S"
        }
        fn eq(&self, rhs: &dyn Object) -> Option<bool> {
            Some(rhs.downcast_ref::<S>()?.0 == self.0)
        }
        fn set(&mut self, _property: &str, value: Value) -> Option<()> {
            match value {
                Value::Integer(v) => self.0 = v,
                _ => panic!(),
            }
            Some(())
        }
    }
    impl core::fmt::Display for S {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{:?}", self)
        }
    }

    unsafe {
        let mut v: Vec<*mut dyn Object> = vec![Box::into_raw(Box::new(Upvalue {
            data: Value::Integer(1),
        }))];

        // ok this seems fine
        let x = v[0];
        x.as_mut().unwrap().downcast_mut::<Upvalue>().unwrap().data = Value::Integer(3);
        assert_eq!(
            x.as_ref().unwrap().downcast_ref::<Upvalue>().unwrap().data,
            Value::Integer(3)
        );
        assert!(std::ptr::eq(v[0], x));

        // wtf
        let y = v[0];
        y.as_mut().unwrap().downcast_mut::<Upvalue>().unwrap().data = Value::Integer(32);
        assert_eq!(
            x.as_ref().unwrap().downcast_ref::<Upvalue>().unwrap().data,
            Value::Integer(32)
        );
        assert_eq!(
            y.as_ref().unwrap().downcast_ref::<Upvalue>().unwrap().data,
            Value::Integer(32)
        );
        assert!(std::ptr::eq(v[0], x) && std::ptr::eq(v[0], y));

        // y and x still refer to the old box, we need to drop it later
        v[0] = Box::into_raw(Box::new(S(32)));
        assert_eq!(v[0].as_ref().unwrap().downcast_ref::<S>().unwrap(), &S(32));
        assert!(!std::ptr::eq(v[0], x) && !std::ptr::eq(v[0], y));

        let v: Vec<Box<dyn Object>> = v.into_iter().map(|p| Box::from_raw(p)).collect();
        drop(v);
        drop(Box::from_raw(y));
    }
}
