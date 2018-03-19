
//#[macro_use]
//extern crate mopa;

use std::any::{Any, TypeId};
use std::cmp;
use value::Value;

//pub trait AnyWrap {
//    fn get_type_id(&self) -> TypeId;
//}
//
//impl<T: Any> AnyWrap for T {
//    fn get_type_id(&self) -> TypeId {
//        TypeId::of::<Self>()
//    }
//}

// cheating
#[repr(C)]
struct TraitObject {
    pub data: *mut (),
    pub vtable: *mut (),
}

pub trait Foreign: Any {
    fn get_name(&self) -> &'static str;
    fn box_clone(&self) -> Box<Foreign>;
    fn compare(&self, rhs: &Foreign) -> Option<cmp::Ordering>;
}

// put it in a separate block because reasons
impl Foreign {
    fn is<T: Foreign>(&self) -> bool {
        //println!("{:?}, {:?}", self.get_type_id(), TypeId::of::<T>());
        self.get_type_id() == TypeId::of::<T>()
        //TypeId::of::<Self>() == TypeId::of::<T>()
    }
    fn downcast_ref<T: Foreign>(&self) -> Option<&T> {
        if self.is::<T>() {
            // cheat
            unsafe {
                // cheaty
                let obj: TraitObject = ::std::mem::transmute(self);
                Some(&*(obj.data as *const T))
            }
        } else {
            None
        }
    }
}

//mopafy!(Foreign);

impl Clone for Box<Foreign> {
    fn clone(&self) -> Box<Foreign> {
        self.box_clone()
    }
}

//macro_rules! impl_cast {
//    ($ty:ident) => {
//        impl $ty {
//            pub fn is<T: $ty>(&self) -> bool {
//                self.get_type_id() == ::std::any::TypeId::of::<T>()
//            }
//            pub fn downcast_rc<T: $ty>(rc: ::std::rc::Rc<Self>) -> Result<::std::rc::Rc<T>, ::std::rc::Rc<Self>> {
//                if rc.is::<T>() {
//                    unsafe {
//                        let obj: TraitObject = ::std::mem::transmute(rc);
//                        Ok(::std::mem::transmute(obj.data))
//                    }
//                } else {
//                    Err(rc)
//                }
//            }
//            pub fn downcast_ref<T: $ty>(&self) -> Option<&T> {
//                if self.is::<T>() {
//                    unsafe {
//                        // cheaty
//                        let obj: TraitObject = ::std::mem::transmute(self);
//                        Some(&*(obj.data as *const T))
//                    }
//                } else {
//                    None
//                }
//            }
//        }
//    }
//}

//impl_cast!(Foreign);

#[derive(Foreign, Debug)]
pub struct Something {
    pub num: i32,
    pub s: String,
}

//#[derive(PartialEq)]
//struct PiccoloString(String);
//
//impl Foreign {
//}

