pub type Value = f64;

pub struct ValueArray {
    values: Vec<Value>,
}

impl ValueArray {
    pub fn new() -> Self {
        Self { values: Vec::new() }
    }

    pub fn write(&mut self, value: Value) -> usize {
        self.values.push(value);
        self.values.len() - 1
    }

    pub fn read(&self, index: usize) -> Value {
        *self.values.get(index).unwrap()
    }

    pub fn free(&mut self) {
        self.values = Vec::new()
    }
}
