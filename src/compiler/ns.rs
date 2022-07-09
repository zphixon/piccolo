use crate::{
    compiler::{
        ast::{Ast, Expr, Stmt},
        Token,
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
}

#[derive(Default)]
struct Namespace<'src> {
    parent: Option<DefaultKey>,
    names: FnvHashMap<&'src str, VariableLocation>,
}

impl std::fmt::Debug for Namespace<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.names.keys()).finish()
    }
}

struct NamespaceRepository<'src> {
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

    fn with_parent(&mut self, ns: DefaultKey) -> DefaultKey {
        self.namespaces.insert(Namespace {
            parent: Some(ns),
            ..Default::default()
        })
    }

    fn find(&self, ns: DefaultKey, name: Token<'src>) -> Option<VariableLocation> {
        debug!("find {}", name.lexeme);

        trace!("{:?}", self.ns(ns).names.keys().collect::<Vec<_>>());
        if self.ns(ns).names.contains_key(name.lexeme) {
            trace!("I have {}", name.lexeme);
            return Some(self.ns(ns).names[name.lexeme]);
        }

        if let Some(parent) = self.ns(ns).parent {
            trace!("checking parent {}", name.lexeme);
            return self.find(parent, name);
        }

        trace!("checking global {}", name.lexeme);
        return self.global_ns().names.get(name.lexeme).cloned();
    }

    fn new_global(&mut self, name: Token<'src>) -> VariableLocation {
        trace!("new global {}", name.lexeme);

        if let Some(location) = self.global_ns().names.get(name.lexeme) {
            return *location;
        }

        let location = VariableLocation::Global(self.global_ns().names.len());
        self.global_ns_mut().names.insert(name.lexeme, location);
        location
    }

    fn new_local(
        &mut self,
        ns: DefaultKey,
        name: Token<'src>,
    ) -> Result<VariableLocation, PiccoloError> {
        trace!("new local {}", name.lexeme);

        if !self.ns(ns).names.contains_key(name.lexeme) {
            // TODO - the variable location should be a reverse index into the stack
            let location = VariableLocation::Local(0);
            trace!("insert {} {:?}", name.lexeme, location);
            self.ns_mut(ns).names.insert(name.lexeme, location);
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
}

impl std::fmt::Debug for NamespaceRepository<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NamespaceRepository")
            .field("globals", self.global_ns())
            .field("children", &self.namespaces.values().collect::<Vec<_>>())
            .finish()
    }
}

pub fn analyze_ns(ast: &Ast) -> Result<(), PiccoloError> {
    let mut repo = NamespaceRepository::new();

    for stmt in ast {
        let global = repo.global_key();
        analyze_ns_stmt(&mut repo, global, stmt)?;
    }

    Ok(())
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

        Stmt::Block { body, .. } => {
            trace!("analyze stmt::block");
            let child = repo.with_parent(ns);
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
                    return Err(PiccoloError::new(ErrorKind::SyntaxError)
                        .msg_string(format!("unknown variable '{}'", variable.lexeme))
                        .pos(variable.pos));
                }
            } else {
                analyze_ns_expr(repo, ns, lval)?;
            }

            analyze_ns_expr(repo, ns, rval)?;
        }

        Stmt::If {
            cond,
            then_block,
            else_block,
            ..
        } => {
            trace!("analyze stmt::if");
            analyze_ns_expr(repo, ns, cond)?;

            let then_child = repo.with_parent(ns);
            for stmt in then_block {
                analyze_ns_stmt(repo, then_child, stmt)?;
            }

            if let Some(else_block) = else_block {
                let else_child = repo.with_parent(ns);
                for stmt in else_block {
                    analyze_ns_stmt(repo, else_child, stmt)?;
                }
            }
        }

        Stmt::While { cond, body, .. } => {
            trace!("analyze stmt::while");
            analyze_ns_expr(repo, ns, cond)?;

            let child = repo.with_parent(ns);
            for stmt in body {
                analyze_ns_stmt(repo, child, stmt)?;
            }
        }

        Stmt::For {
            init,
            cond,
            name,
            inc_expr,
            body,
            ..
        } => {
            trace!("analyze stmt::for");
            let child = repo.with_parent(ns);
            repo.new_variable(ns, *name)?;
            analyze_ns_stmt(repo, child, init)?;
            analyze_ns_expr(repo, child, cond)?;
            analyze_ns_expr(repo, child, inc_expr)?;
            let inner = repo.with_parent(child);
            for stmt in body {
                analyze_ns_stmt(repo, inner, stmt)?;
            }
        }

        Stmt::ForEach { .. } => todo!(),

        Stmt::Fn {
            name, args, body, ..
        } => {
            repo.new_variable(ns, *name)?;
            let body_ns = repo.with_parent(ns);
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
            let data_ns = repo.with_parent(ns);
            for field in fields {
                analyze_ns_stmt(repo, data_ns, field)?;
            }
            for method in methods {
                analyze_ns_stmt(repo, data_ns, method)?;
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
        // Expr::Get { object, name } => {}
        // Expr::Set { object, name, value } => {}
        // Expr::Index { right_bracket, object, index } => {}
        Expr::Fn { args, body, .. } => {
            trace!("analyze expr::fn");
            let child = repo.with_parent(ns);
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
    fn _99b() {
        let src = std::fs::read_to_string("examples/99b.pc").unwrap();
        let ast = crate::compiler::parser::parse(&src).unwrap();
        let _ = analyze_ns(&ast).unwrap();
    }

    #[test]
    fn namespace() {
        let mut repo = NamespaceRepository::new();
        repo.new_global(Token::identifier("wow"));

        {
            let child = repo.with_parent(repo.global_key());
            assert!(repo.find(child, Token::identifier("wow")).is_some());
            let local = repo.new_local(child, Token::identifier("bean")).unwrap();

            {
                let child2 = repo.with_parent(child);
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
