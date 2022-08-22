pub struct Local {
    pub depth: Option<usize>,
    pub name: String,
    pub is_captured: bool,
}

impl Local {
    pub fn new(name: String) -> Self {
        Self {
            depth: None,
            name: name,
            is_captured: false,
        }
    }
    pub fn initialize(&mut self, depth: usize) {
        self.depth = Some(depth)
    }
}
