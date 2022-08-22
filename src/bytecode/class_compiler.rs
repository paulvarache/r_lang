pub type ClassCompilerLink = Option<Box<ClassCompiler>>;

pub struct ClassCompiler {
    pub enclosing: ClassCompilerLink,
}

impl ClassCompiler {
    pub fn new(enclosing: ClassCompilerLink) -> Self {
        Self { enclosing }
    }
}
