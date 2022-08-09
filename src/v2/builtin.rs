use super::Func;

#[derive(Clone)]
pub struct Print {}

impl Func for Print {
    fn call(&mut self, _state: &mut super::State) -> Result<(), crate::error::PiccoloError> {
        todo!()
    }
}
