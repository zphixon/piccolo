use std::hash::Hash;

use crate::{debug, trace};
use fnv::FnvHashMap;
use slotmap::{Key, KeyData, SlotMap};

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct StringPtr {
    data: KeyData,
    pub len: usize,
}

impl PartialOrd for StringPtr {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.data.partial_cmp(&other.data)
    }
}

impl Ord for StringPtr {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.data.cmp(&other.data)
    }
}

impl Eq for StringPtr {}

impl Hash for StringPtr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.data.hash(state)
    }
}

unsafe impl Key for StringPtr {
    fn data(&self) -> slotmap::KeyData {
        self.data
    }
}

impl From<KeyData> for StringPtr {
    fn from(data: KeyData) -> Self {
        StringPtr { data, len: 0 }
    }
}

#[derive(Default, Debug)]
pub struct Interner {
    table: FnvHashMap<&'static str, StringPtr>,
    strings: SlotMap<StringPtr, String>,
}

impl Interner {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn allocate_string(&mut self, string: String) -> StringPtr {
        debug!("intern string {string}");
        if let Some(ptr) = self.get_string_ptr(&string) {
            trace!("already had {string}");
            return ptr;
        }

        self.insert(string)
    }

    pub fn allocate_str(&mut self, string: &str) -> StringPtr {
        debug!("intern str {string}");
        if let Some(ptr) = self.get_string_ptr(string) {
            trace!("already had {string}");
            return ptr;
        }

        self.insert(string.to_string())
    }

    fn insert(&mut self, string: String) -> StringPtr {
        use unicode_segmentation::UnicodeSegmentation;
        let len = string.graphemes(true).count();
        trace!("len is {len}");

        let mut ptr = self.strings.insert(string);

        ptr.len = len;
        self.table.insert(
            // SAFETY: References handed out by get_string() only last as long as the Interner.
            // There's no way to get at the supposedly 'static strs in the public api.
            unsafe { std::mem::transmute::<&str, &'static str>(self.strings[ptr].as_str()) },
            ptr,
        );

        ptr
    }

    pub fn get_string(&self, ptr: StringPtr) -> &str {
        self.strings
            .get(ptr)
            .map(|string| string.as_str())
            .expect("invalid string pointer")
    }

    pub fn get_string_ptr(&self, string: &str) -> Option<StringPtr> {
        self.table.get(string).cloned()
    }

    pub fn strings(&self) -> impl Iterator<Item = &str> {
        self.table.keys().cloned()
    }

    pub fn num_strings(&self) -> usize {
        self.strings.len()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn intern() {
        let mut interner = Interner::default();
        let wow = interner.allocate_string(String::from("wow!"));
        assert_eq!("wow!", interner.get_string(wow));
    }

    #[test]
    fn intern2() {
        use rand::Rng;
        const NUM_RUNS: usize = 1250;
        const NUM_STRINGS: usize = 32;
        const STRING_LENGTH: usize = 5;

        for run in 1..=NUM_RUNS {
            println!("run {run}");
            let mut interner = Interner::default();
            let mut ptrs = Vec::with_capacity(NUM_STRINGS);

            for _ in 1..=NUM_STRINGS {
                let mut string = String::with_capacity(STRING_LENGTH);
                for _ in 1..=STRING_LENGTH {
                    string.push(rand::thread_rng().gen_range('a'..='b'));
                }

                let ptr = interner.allocate_string(string.clone());
                if ptrs.iter().all(|contained: &StringPtr| contained != &ptr) {
                    ptrs.push(ptr);
                    println!("    unique {string}");
                } else {
                    println!("not unique {string}");
                }
            }

            // Pigeonhole priciple
            assert_ne!(NUM_STRINGS, ptrs.len())
        }
    }
}
