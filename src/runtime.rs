//! Modules for the runtime representation and interpretation of Piccolo bytecode.

pub mod chunk;
pub mod memory;
pub mod op;
pub mod value;
pub mod vm;

#[test]
fn ptr_funnies() {
    use value::Object;
    unsafe {
        let mut v: Vec<*mut dyn Object> = vec![Box::into_raw(Box::new(1))];

        // ok this seems fine
        let x = v[0];
        *x.as_mut().unwrap().downcast_mut::<i64>().unwrap() = 3;
        assert_eq!(x.as_ref().unwrap().downcast_ref::<i64>().unwrap(), &3);
        assert!(std::ptr::eq(v[0], x));

        // wtf
        let y = v[0];
        *y.as_mut().unwrap().downcast_mut::<i64>().unwrap() = 32;
        assert_eq!(x.as_ref().unwrap().downcast_ref::<i64>().unwrap(), &32);
        assert_eq!(y.as_ref().unwrap().downcast_ref::<i64>().unwrap(), &32);
        assert!(std::ptr::eq(v[0], x) && std::ptr::eq(v[0], y));

        // y and x still refer to the old box, we need to drop it later
        v[0] = Box::into_raw(Box::new(String::from("haha")));
        assert_eq!(
            v[0].as_ref().unwrap().downcast_ref::<String>().unwrap(),
            &String::from("haha")
        );
        assert!(!std::ptr::eq(v[0], x) && !std::ptr::eq(v[0], y));

        let v: Vec<Box<dyn Object>> = v.into_iter().map(|p| Box::from_raw(p)).collect();
        drop(v);
        drop(Box::from_raw(y));
    }
}
