/// Scope management utilities

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScopeKind {
    Global,
    Procedure(String),
    Block,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub kind: ScopeKind,
    pub parent: Option<Box<Scope>>,
}

impl Scope {
    pub fn global() -> Self {
        Self {
            kind: ScopeKind::Global,
            parent: None,
        }
    }

    pub fn procedure(name: String, parent: Scope) -> Self {
        Self {
            kind: ScopeKind::Procedure(name),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn block(parent: Scope) -> Self {
        Self {
            kind: ScopeKind::Block,
            parent: Some(Box::new(parent)),
        }
    }

    pub fn is_global(&self) -> bool {
        matches!(self.kind, ScopeKind::Global)
    }

    pub fn is_procedure(&self) -> bool {
        matches!(self.kind, ScopeKind::Procedure(_))
    }

    pub fn procedure_name(&self) -> Option<&str> {
        match &self.kind {
            ScopeKind::Procedure(name) => Some(name),
            _ => None,
        }
    }
}
