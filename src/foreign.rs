
use std::any::{Any, TypeId};
use std::cmp;

// cheating
#[repr(C)]
pub struct TraitObject {
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
    pub fn is<T: Foreign>(&self) -> bool {
        self.get_type_id() == TypeId::of::<T>()
    }
    pub fn downcast_ref<T: Foreign>(&self) -> Option<&T> {
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

impl Clone for Box<Foreign> {
    fn clone(&self) -> Box<Foreign> {
        self.box_clone()
    }
}

#[derive(Foreign, Debug)]
pub struct Test {
    inner: String,
}

