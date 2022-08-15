#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: (usize, usize),
    pub end: (usize, usize),
}

impl Span {
    pub fn new(start_line: usize, start_col: usize, end_line: usize, end_col: usize) -> Self {
        Self {
            start: (start_line, start_col),
            end: (end_line, end_col),
        }
    }

    pub(crate) fn new_from_range(start: Span, end: Span) -> Span {
        Self {
            start: start.start,
            end: end.end,
        }
    }
}

impl Eq for Span {

}

impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        self.start == other.start && self.end == other.end
    }
}
impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.start.0.partial_cmp(&other.start.0).and_then(|o| {
            let s = self.start.1.partial_cmp(&other.start.1);
            s.map(|s| o.then(s))
        })
    }
}

impl Ord for Span {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.start.0.cmp(&other.start.0).then_with(|| self.start.1.cmp(&other.start.1))
    }
}