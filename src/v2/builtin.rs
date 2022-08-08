use super::Func;

pub struct Print {}
impl Func for Print {
    fn call(&mut self, state: &mut super::State) -> Result<(), crate::error::PiccoloError> {
        todo!()
    }
}
