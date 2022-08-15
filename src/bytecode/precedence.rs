use r_ast::EnumIndex;

#[derive(Default, EnumIndex)]
pub enum Precedence {
    #[default]
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl PartialEq for Precedence {
    fn eq(&self, other: &Self) -> bool {
        self.get_enum_index() == other.get_enum_index()
    }
}
impl PartialOrd for Precedence {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.get_enum_index().partial_cmp(&other.get_enum_index())
    }
}
