use crate::runtime::memory2::Ptr;
use fnv::FnvHashMap;
use slotmap::SlotMap;

#[derive(Default, Debug)]
pub struct Interner {
    table: FnvHashMap<&'static str, Ptr>,
    strings: SlotMap<Ptr, String>,
}

impl Interner {
    pub fn alloc_string(&mut self, string: String) -> Ptr {
        if let Some(ptr) = self.table.get(string.as_str()) {
            return *ptr;
        }

        let ptr = self.strings.insert(string);
        self.table.insert(
            // SAFETY: References handed out by get_string() only last as long as the Interner.
            // There's no way to get at the supposedly 'static strs in the public api.
            unsafe { std::mem::transmute(self.strings[ptr].as_str()) },
            ptr,
        );
        ptr
    }

    pub fn get_string(&self, ptr: Ptr) -> Option<&str> {
        self.strings.get(ptr).map(|string| string.as_str())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn intern() {
        let mut interner = Interner::default();
        let wow = interner.alloc_string(String::from("wow!"));
        assert_eq!("wow!", interner.get_string(wow).unwrap());
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

                let ptr = interner.alloc_string(string.clone());
                if ptrs.iter().all(|contained: &Ptr| contained != &ptr) {
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
