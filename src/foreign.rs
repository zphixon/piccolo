
use std::fmt;

pub trait Foreign {
    fn box_clone(&self) -> Box<Foreign>;
    fn partial_eq(&self, rhs: &Box<Foreign>) -> bool;
    fn get_name(&self) -> &'static str;
}

impl Clone for Box<Foreign> {
    fn clone(&self) -> Box<Foreign> {
        self.box_clone()
    }
}

impl PartialEq for Box<Foreign> {
    fn eq(&self, rhs: &Box<Foreign>) -> bool {
        self.partial_eq(rhs)
    }
}

