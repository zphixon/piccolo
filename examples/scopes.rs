#[derive(Debug)]
struct Local {
    name: String,
    slot: usize,
}

#[derive(Debug)]
struct Scope {
    names: Vec<Vec<Local>>,
}

impl Scope {
    fn new() -> Self {
        Self {
            names: vec![vec![]],
        }
    }

    fn depth(&self) -> usize {
        self.names.len()
    }

    /// Get the depth of a name.
    ///
    /// Returns Some if the name existed, None if it did not.
    fn get_depth(&self, name: &str) -> Option<usize> {
        self.names
            .iter()
            .enumerate()
            .rev()
            .find(|(_, names)| names.iter().any(|local| local.name == name))
            .map(|(depth, _)| depth + 1)
    }

    /// Add a name to the current scope.
    ///
    /// Returns None if the name does not exist, Some depth if it does.
    fn add_name(&mut self, name: &str) -> Option<usize> {
        let slot = self.len();
        self.get_depth(name)
            .and_then(|depth| {
                if depth < self.depth() {
                    // if the name exists and it's in a higher scope, ignore it and shadow
                    None
                } else {
                    // otherwise return its depth, we already defined it
                    Some(depth)
                }
            })
            .or_else(|| {
                self.names.last_mut().unwrap().push(Local {
                    name: name.to_string(),
                    slot,
                });
                None
            })
    }

    fn get_slot(&self, name: &str) -> Option<usize> {
        for scope in self.names.iter().rev() {
            for local in scope {
                if local.name == name {
                    return Some(local.slot);
                }
            }
        }
        None
    }

    fn push(&mut self) {
        self.names.push(Vec::new());
    }

    fn pop(&mut self) -> Vec<Local> {
        self.names.pop().unwrap()
    }

    fn len(&self) -> usize {
        let mut len = 0;
        for scope in &self.names {
            for _name in scope {
                len += 1;
            }
        }
        len
    }
}

fn main() {
    let mut scope = Scope::new();
    assert_eq!(None, scope.add_name("hello"));
    assert_eq!(Some(1), scope.get_depth("hello"));
    assert_eq!(Some(1), scope.add_name("hello"));

    assert_eq!(Some(0), scope.get_slot("hello"));

    scope.push();

    assert_eq!(Some(1), scope.get_depth("hello"));
    assert_eq!(None, scope.add_name("hello"));
    assert_eq!(Some(2), scope.get_depth("hello"));
    assert_eq!(Some(2), scope.add_name("hello"));

    assert_eq!(None, scope.add_name("world"));
    assert_eq!(Some(2), scope.add_name("world"));

    assert_eq!(Some(1), scope.get_slot("hello"));
    assert_eq!(Some(2), scope.get_slot("world"));

    scope.push();

    assert_eq!(Some(1), scope.get_slot("hello"));
    assert_eq!(Some(2), scope.get_slot("world"));

    assert_eq!(None, scope.add_name("hello"));
    assert_eq!(None, scope.add_name("world"));

    assert_eq!(Some(3), scope.get_slot("hello"));
    assert_eq!(Some(4), scope.get_slot("world"));

    scope.pop();

    assert_eq!(Some(1), scope.get_slot("hello"));
    assert_eq!(Some(2), scope.get_slot("world"));

    println!("{:#?}", scope);
    println!("all good bruv");
}
