pub type ClassCompilerLink = Option<Box<ClassCompiler>>;

pub struct ClassCompiler {
    pub enclosing: ClassCompilerLink,
    pub has_superclass: bool,
}

impl ClassCompiler {
    pub fn new(enclosing: ClassCompilerLink) -> Self {
        Self {
            enclosing,
            has_superclass: false,
        }
    }
}
