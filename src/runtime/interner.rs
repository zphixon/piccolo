use fnv::FnvHashMap;
use slotmap::{DefaultKey, SlotMap};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct StringPtr {
    key: DefaultKey,
    pub len: usize,
}

#[derive(Default, Debug)]
pub struct Interner {
    table: FnvHashMap<&'static str, DefaultKey>,
    strings: SlotMap<DefaultKey, String>,
}

impl Interner {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn allocate_string(&mut self, string: String) -> StringPtr {
        if let Some(ptr) = self.get_string_ptr(&string) {
            return ptr;
        }

        self.insert(string)
    }

    pub fn allocate_str(&mut self, string: &str) -> StringPtr {
        if let Some(ptr) = self.get_string_ptr(string) {
            return ptr;
        }

        self.insert(string.to_string())
    }

    fn insert(&mut self, string: String) -> StringPtr {
        use unicode_segmentation::UnicodeSegmentation;
        let len = string.graphemes(true).count();

        let ptr = self.strings.insert(string);
        self.table.insert(
            // SAFETY: References handed out by get_string() only last as long as the Interner.
            // There's no way to get at the supposedly 'static strs in the public api.
            unsafe { std::mem::transmute(self.strings[ptr].as_str()) },
            ptr,
        );

        StringPtr { key: ptr, len }
    }

    pub fn get_string(&self, ptr: StringPtr) -> &str {
        self.strings
            .get(ptr.key)
            .map(|string| string.as_str())
            .expect("invalid string pointer")
    }

    pub fn get_string_ptr(&self, string: &str) -> Option<StringPtr> {
        use unicode_segmentation::UnicodeSegmentation;
        let len = string.graphemes(true).count();

        self.table
            .get(string)
            .cloned()
            .map(|key| StringPtr { key, len })
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
