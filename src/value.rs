use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct Value(pub f32);

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
