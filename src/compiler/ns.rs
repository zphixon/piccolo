use crate::{
    compiler::{
        ast::{Ast, Expr, Stmt},
        Pos, Token,
    },
    debug,
    error::{ErrorKind, PiccoloError},
    trace,
};
use fnv::FnvHashMap;
use slotmap::{DefaultKey, SlotMap};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum VariableLocation {
    Global(usize),
    Local(usize),
    Capture(usize),
}

#[derive(Default)]
struct Namespace<'src> {
    parent: Option<DefaultKey>,
    names: FnvHashMap<Token<'src>, VariableLocation>,
    children: Vec<DefaultKey>,
    start: Pos,
    captures: bool,
}

impl std::fmt::Debug for Namespace<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.names.keys()).finish()
    }
}

pub struct NamespaceRepository<'src> {
    namespaces: SlotMap<DefaultKey, Namespace<'src>>,
    global: DefaultKey,
}

impl<'src> NamespaceRepository<'src> {
    fn new() -> Self {
        let mut namespaces = SlotMap::default();
        let global = namespaces.insert(Default::default());

        let mut repo = NamespaceRepository { namespaces, global };
        repo.new_global(Token::identifier("print"));

        repo
    }

    fn global_key(&self) -> DefaultKey {
        self.global
    }

    fn global_ns(&self) -> &Namespace<'src> {
        &self.namespaces[self.global_key()]
    }

    fn global_ns_mut<'this>(&'this mut self) -> &'this mut Namespace<'src> {
        let bruh = self.global_key();
        &mut self.namespaces[bruh]
    }

    fn ns<'this>(&'this self, ns: DefaultKey) -> &'this Namespace<'src> {
        &self.namespaces[ns]
    }

    fn ns_mut<'this>(&'this mut self, ns: DefaultKey) -> &'this mut Namespace<'src> {
        &mut self.namespaces[ns]
    }

    fn with_parent(&mut self, ns: DefaultKey, start: Pos) -> DefaultKey {
        let key = self.namespaces.insert(Namespace {
            parent: Some(ns),
            start,
            ..Default::default()
        });
        self.namespaces[ns].children.push(key);
        key
    }

    fn with_parent_captures(&mut self, ns: DefaultKey, start: Pos) -> DefaultKey {
        let key = self.with_parent(ns, start);
        self.ns_mut(key).captures = true;
        key
    }

    fn find(&mut self, ns: DefaultKey, name: Token<'src>) -> Option<VariableLocation> {
        debug!("find {}", name.lexeme);

        trace!("{:?}", self.ns(ns).names.keys().collect::<Vec<_>>());
        if self.ns(ns).names.contains_key(&name) {
            trace!("I have {}", name.lexeme);
            return Some(self.ns(ns).names[&name]);
        }

        if let Some(parent) = self.ns(ns).parent {
            trace!("checking parent for {}", name.lexeme);
            let loc = self.find(parent, name);

            if let Some(VariableLocation::Local(_)) = loc {
                if self.ns(ns).captures {
                    debug!("capturing {}", name.lexeme);
                    self.ns_mut(ns)
                        .names
                        .insert(name, VariableLocation::Capture(0));
                }
            }

            return loc;
        }

        trace!("checking global ns for {}", name.lexeme);
        return self.global_ns().names.get(&name).cloned();
    }

    fn new_global(&mut self, name: Token<'src>) -> VariableLocation {
        trace!("new global {}", name.lexeme);

        if let Some(location) = self.global_ns().names.get(&name) {
            return *location;
        }

        let location = VariableLocation::Global(self.global_ns().names.len());
        self.global_ns_mut().names.insert(name, location);
        location
    }

    fn new_local(
        &mut self,
        ns: DefaultKey,
        name: Token<'src>,
    ) -> Result<VariableLocation, PiccoloError> {
        trace!("new local {}", name.lexeme);

        if !self.ns(ns).names.contains_key(&name) {
            // TODO - the variable location should be a reverse index into the stack
            let location = VariableLocation::Local(0);
            trace!("insert {} {:?}", name.lexeme, location);
            self.ns_mut(ns).names.insert(name, location);
            return Ok(location);
        }

        Err(PiccoloError::new(ErrorKind::SyntaxError)
            .msg_string(format!("local already defined: '{}'", name.lexeme))
            .pos(name.pos))
    }

    fn new_variable(
        &mut self,
        ns: DefaultKey,
        name: Token<'src>,
    ) -> Result<VariableLocation, PiccoloError> {
        debug!("new var {}", name.lexeme);
        if ns == self.global_key() {
            Ok(self.new_global(name))
        } else {
            self.new_local(ns, name)
        }
    }

    pub fn hmm(&self) {
        self.hmm_rec(0, self.global_key())
    }

    fn hmm_rec(&self, indent: usize, ns: DefaultKey) {
        print!("{:<10}", format!("{}", self.ns(ns).start));
        print!("{:>indent$}[", "");
        for (i, (name, loc)) in self.ns(ns).names.iter().enumerate() {
            print!("{}", name.lexeme);
            if let VariableLocation::Capture(_) = loc {
                print!(" (from {})", name.pos);
            }
            if i + 1 != self.ns(ns).names.len() {
                print!(", ");
            }
        }
        println!("]");

        for child in self.ns(ns).children.iter() {
            self.hmm_rec(indent + 2, *child);
        }
    }
}

impl std::fmt::Debug for NamespaceRepository<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NamespaceRepository")
            .field("globals", self.global_ns())
            .field("children", &self.namespaces.values().collect::<Vec<_>>())
            .finish()
    }
}

pub fn analyze_ns<'src>(ast: &Ast<'src>) -> Result<NamespaceRepository<'src>, PiccoloError> {
    let mut repo = NamespaceRepository::new();

    for stmt in ast {
        let global = repo.global_key();
        analyze_ns_stmt(&mut repo, global, stmt)?;
    }

    Ok(repo)
}

fn err(variable: Token) -> Result<(), PiccoloError> {
    return Err(PiccoloError::new(ErrorKind::SyntaxError)
        .msg_string(format!("unknown variable '{}'", variable.lexeme))
        .pos(variable.pos));
}

fn analyze_ns_stmt<'src>(
    repo: &mut NamespaceRepository<'src>,
    ns: DefaultKey,
    stmt: &Stmt<'src>,
) -> Result<(), PiccoloError> {
    match stmt {
        Stmt::Expr { expr, .. } => {
            trace!("analyze stmt::expr");
            analyze_ns_expr(repo, ns, expr)?;
        }

        Stmt::Block { do_, body, .. } => {
            trace!("analyze stmt::block");
            let child = repo.with_parent(ns, do_.pos);
            for stmt in body {
                analyze_ns_stmt(repo, child, stmt)?;
            }
        }

        Stmt::Declaration { name, value, .. } => {
            trace!("analyze stmt::declaration");
            analyze_ns_expr(repo, ns, value)?;
            repo.new_variable(ns, *name)?;
        }

        Stmt::Assignment { lval, rval, .. } => {
            trace!("analyze stmt::assignment");
            if let Expr::Variable { variable } = lval {
                if repo.find(ns, *variable).is_none() {
                    return err(*variable);
                }
            } else {
                analyze_ns_expr(repo, ns, lval)?;
            }

            analyze_ns_expr(repo, ns, rval)?;
        }

        Stmt::If {
            if_,
            cond,
            then_block,
            else_,
            else_block,
            ..
        } => {
            trace!("analyze stmt::if");
            analyze_ns_expr(repo, ns, cond)?;

            let then_child = repo.with_parent(ns, if_.pos);
            for stmt in then_block {
                analyze_ns_stmt(repo, then_child, stmt)?;
            }

            if let (Some(else_), Some(else_block)) = (else_, else_block) {
                let else_child = repo.with_parent(ns, else_.pos);
                for stmt in else_block {
                    analyze_ns_stmt(repo, else_child, stmt)?;
                }
            }
        }

        Stmt::While {
            while_, cond, body, ..
        } => {
            trace!("analyze stmt::while");
            analyze_ns_expr(repo, ns, cond)?;

            let child = repo.with_parent(ns, while_.pos);
            for stmt in body {
                analyze_ns_stmt(repo, child, stmt)?;
            }
        }

        Stmt::For {
            for_,
            init,
            cond,
            name,
            inc_expr,
            do_,
            body,
            ..
        } => {
            trace!("analyze stmt::for");
            let child = repo.with_parent(ns, for_.pos);
            analyze_ns_stmt(repo, child, init)?;
            analyze_ns_expr(repo, child, cond)?;
            if repo.find(child, *name).is_none() {
                return err(*name);
            }
            analyze_ns_expr(repo, child, inc_expr)?;
            let inner = repo.with_parent(child, do_.pos);
            for stmt in body {
                analyze_ns_stmt(repo, inner, stmt)?;
            }
        }

        Stmt::ForEach {
            for_,
            item,
            iter,
            body,
            ..
        } => {
            trace!("analyze stmt::foreach");
            if repo.find(ns, *iter).is_none() {
                return err(*iter);
            }

            let child = repo.with_parent(ns, for_.pos);
            repo.new_variable(child, *item)?;
            for stmt in body {
                analyze_ns_stmt(repo, child, stmt)?;
            }
        }

        Stmt::Fn {
            name, args, body, ..
        } => {
            repo.new_variable(ns, *name)?;
            let body_ns = repo.with_parent_captures(ns, name.pos);
            for arg in args {
                repo.new_variable(body_ns, *arg)?;
            }
            for stmt in body {
                analyze_ns_stmt(repo, body_ns, stmt)?;
            }
        }

        Stmt::Break { .. } => {}

        Stmt::Continue { .. } => {}

        Stmt::Retn { value, .. } => {
            if let Some(value) = value {
                analyze_ns_expr(repo, ns, value)?;
            }
        }

        Stmt::Assert { value, .. } => {
            analyze_ns_expr(repo, ns, value)?;
        }

        Stmt::Data {
            name,
            methods,
            fields,
        } => {
            repo.new_variable(ns, *name)?;
            let data_ns = repo.with_parent(ns, name.pos);
            for field in fields {
                analyze_ns_stmt(repo, data_ns, field)?;
            }
            for method in methods {
                let method_ns = repo.with_parent(data_ns, method.token().pos);
                analyze_ns_stmt(repo, method_ns, method)?;
            }
        }
    }

    Ok(())
}

fn analyze_ns_expr<'src>(
    repo: &mut NamespaceRepository<'src>,
    ns: DefaultKey,
    expr: &Expr<'src>,
) -> Result<(), PiccoloError> {
    match expr {
        Expr::Literal { .. } => {}

        Expr::Paren { expr, .. } => {
            trace!("analyze expr::paren");
            analyze_ns_expr(repo, ns, expr)?;
        }

        Expr::Variable { variable } => {
            trace!("analyze expr::variable");
            if repo.find(ns, *variable).is_none() {
                return Err(PiccoloError::new(ErrorKind::SyntaxError)
                    .msg_string(format!("unknown variable '{}'", variable.lexeme))
                    .pos(variable.pos));
            }
        }

        Expr::Unary { rhs, .. } => {
            trace!("analyze expr::unary");
            analyze_ns_expr(repo, ns, rhs)?;
        }

        Expr::Binary { lhs, rhs, .. } => {
            trace!("analyze expr::binary");
            analyze_ns_expr(repo, ns, lhs)?;
            analyze_ns_expr(repo, ns, rhs)?;
        }

        Expr::Logical { lhs, rhs, .. } => {
            trace!("analyze expr::logical");
            analyze_ns_expr(repo, ns, lhs)?;
            analyze_ns_expr(repo, ns, rhs)?;
        }

        Expr::Call { callee, args, .. } => {
            trace!("analyze expr::call");
            analyze_ns_expr(repo, ns, callee)?;
            for arg in args {
                analyze_ns_expr(repo, ns, arg)?;
            }
        }

        // Expr::New { name, args } => {}
        Expr::Get { object, .. } => {
            analyze_ns_expr(repo, ns, object)?;
        }

        Expr::Index { object, index, .. } => {
            analyze_ns_expr(repo, ns, object)?;
            analyze_ns_expr(repo, ns, index)?;
        }

        Expr::Fn {
            fn_, args, body, ..
        } => {
            trace!("analyze expr::fn");
            let child = repo.with_parent_captures(ns, fn_.pos);
            for arg in args {
                repo.new_variable(child, *arg)?;
            }
            for stmt in body {
                analyze_ns_stmt(repo, child, stmt)?;
            }
        }

        _ => return Err(PiccoloError::todo(format!("analyze_ns: {expr:#?}"))),
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn files() {
        #[cfg(feature = "log")]
        my_log::init();

        for file in [
            "examples/99b.pc",
            "examples/test_files/closure/simple.pc",
            "examples/test_files/closure/nested_closure.pc",
            "examples/test_files/closure/assign_to_closure.pc",
        ] {
            println!("{file}");
            let src = std::fs::read_to_string(file).unwrap();
            let ast = crate::compiler::parser::parse(&src).unwrap();
            let repo = analyze_ns(&ast).unwrap();
            repo.hmm();
        }
    }

    #[test]
    fn namespace() {
        let mut repo = NamespaceRepository::new();
        repo.new_global(Token::identifier("wow"));

        {
            let child = repo.with_parent(repo.global_key(), Default::default());
            assert!(repo.find(child, Token::identifier("wow")).is_some());
            let local = repo.new_local(child, Token::identifier("bean")).unwrap();

            {
                let child2 = repo.with_parent(child, Default::default());
                assert_eq!(repo.find(child2, Token::identifier("bean")).unwrap(), local);
                let local_shadow = repo.new_local(child2, Token::identifier("bean")).unwrap();
                assert_eq!(
                    repo.find(child2, Token::identifier("bean")).unwrap(),
                    local_shadow
                );
                assert_ne!(repo.find(child2, Token::identifier("bean")).unwrap(), local);
            }
        }
    }
}
