pub type Value = f64;

pub struct ValueArray {
    values: Vec<Value>,
}

impl ValueArray {
    pub fn new() -> Self {
        Self { values: Vec::new() }
    }

    pub fn write(&mut self, value: Value) -> u8 {
        self.values.push(value);
        (self.values.len() - 1).try_into().unwrap()
    }

    pub fn read(&self, index: u8) -> Value {
        *self.values.get(index as usize).unwrap()
    }

    pub fn free(&mut self) {
        self.values = Vec::new()
    }
}
