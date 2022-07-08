use fnv::FnvHashMap;
use slotmap::{DefaultKey, SlotMap};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct StringPtr(DefaultKey);

#[derive(Debug)]
pub struct Interner {
    table: FnvHashMap<&'static str, DefaultKey>,
    strings: SlotMap<DefaultKey, String>,
}

impl Default for Interner {
    fn default() -> Self {
        Interner::new()
    }
}

impl Interner {
    pub fn new() -> Self {
        let mut interner = Interner {
            table: Default::default(),
            strings: Default::default(),
        };

        interner.allocate_string(String::from("print"));
        interner.allocate_string(String::from("len"));

        interner
    }

    pub fn allocate_string(&mut self, string: String) -> StringPtr {
        if let Some(ptr) = self.table.get(string.as_str()) {
            return StringPtr(*ptr);
        }

        let ptr = self.strings.insert(string);
        self.table.insert(
            // SAFETY: References handed out by get_string() only last as long as the Interner.
            // There's no way to get at the supposedly 'static strs in the public api.
            unsafe { std::mem::transmute(self.strings[ptr].as_str()) },
            ptr,
        );
        StringPtr(ptr)
    }

    pub fn get_string(&self, ptr: StringPtr) -> &str {
        self.strings
            .get(ptr.0)
            .map(|string| string.as_str())
            .expect("invalid string pointer")
    }

    pub fn get_string_ptr(&self, string: &str) -> Option<StringPtr> {
        self.table.get(string).cloned().map(StringPtr)
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
