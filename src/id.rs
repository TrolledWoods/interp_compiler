use std::sync::atomic::{AtomicU32, Ordering};

pub type Id = u32;

#[derive(Debug)]
pub struct IdBuilder(AtomicU32);

impl IdBuilder {
    pub fn new() -> IdBuilder {
        IdBuilder(AtomicU32::new(0))
    }

    pub fn create_id(&self) -> Id {
        self.0.fetch_add(1, Ordering::SeqCst)
    }
}
