use crate::prelude::*;
use std::fmt;
use std::ops::Range;

#[derive(Clone, Copy, PartialEq)]
pub struct Pos {
    pub file: TinyString,

    pub start: (usize, usize),

    /// The line number of the end should always be
    /// greater or equal to the line of the start,
    /// and if they are the same, the character
    /// should be greater(not equal) to the character
    /// of the start.
    pub end: (usize, usize),
}

impl Pos {
    /// Creates a new position.
    ///
    /// # Panics
    /// If the end is before the start.
    pub fn new(
        file: impl Into<TinyString>,
        start: (usize, usize),
        end: (usize, usize),
    ) -> Pos {
        assert!(
            (start.0 == end.0 && start.1 < end.1)
                || start.0 < end.0,
            "Starting point cannot be after the end point"
        );

        Pos {
            file: file.into(),
            start,
            end,
        }
    }

    pub fn single(
        file: impl Into<TinyString>,
        pos: (usize, usize),
    ) -> Pos {
        Pos {
            file: file.into(),
            start: pos,
            end: (pos.0, pos.1 + 1),
        }
    }

    #[inline(always)]
    pub fn start_line(&self) -> usize {
        self.start.0
    }

    #[inline(always)]
    pub fn start_char(&self) -> usize {
        self.start.1
    }

    #[inline(always)]
    pub fn end_line(&self) -> usize {
        self.start.0
    }

    #[inline(always)]
    pub fn end_char(&self) -> usize {
        self.start.1
    }

    pub fn join(&self, other: Pos) -> Pos {
        assert_eq!(
            self.file, other.file,
            "Cannot join two Pos's with different files"
        );
        assert!(
            self.start_line() < other.end_line()
                || (self.start_line() == other.end_line()
                    && self.start_char()
                        <= other.end_char()),
            "Cannot join in the wrong order"
        );

        Pos {
            file: self.file.clone(),
            start: self.start,
            end: other.end,
        }
    }
}

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "({}:{}-{}:{})",
            self.start.0 + 1,
            self.start.1 + 1,
            self.end.0 + 1,
            self.end.1 + 1
        )
    }
}

impl PartialEq<Range<(usize, usize)>> for Pos {
    fn eq(&self, other: &Range<(usize, usize)>) -> bool {
        self.start == other.start && self.end == other.end
    }
}
