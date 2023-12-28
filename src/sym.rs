use std::collections::{HashMap, HashSet};

pub struct SymbolTable {
    inner: HashSet<String>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            inner: Default::default(),
        }
    }

    pub fn add(&mut self, symbol: String) {
        self.inner.insert(symbol);
    }

    pub fn contains(&self, symbol: &str) -> bool {
        self.inner.contains(symbol)
    }

    pub fn iter(&self) -> impl Iterator<Item = &String> {
        self.inner.iter()
    }
}
