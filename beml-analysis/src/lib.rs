#![feature(box_patterns)]

use beml_tree::{
    abstr::Declaration,
    errors::{CompilerPass, StepFailedError},
};

pub mod lowering;
pub mod typing;

/// Infer a file into a [hir::File]
pub fn lower_to_hir(file: beml_tree::abstr::File) -> miette::Result<beml_tree::hir::File> {
    let mut env = typing::TypeEnv::new(file.source);
    let mut definitions = im::HashMap::default();
    let defers = file
        .declarations
        .into_iter()
        .map(|(name, decl)| (name, typing::decl::infer_decl(&mut env, decl)))
        .collect::<Vec<_>>();

    for (name, typing::decl::Defer(f)) in defers {
        if let Some(value) = f(&mut env) {
            definitions.insert(name.text, value);
        }
    }

    if env.errors.borrow().is_empty() {
        Ok(beml_tree::hir::File {
            source: env.data,
            algebraic_data_types: env.types.into_iter().collect(),
            definitions,
        })
    } else {
        Err(StepFailedError {
            compiler_pass: CompilerPass::TypeChecking,

            // this is safe, because we never use the `ctx.errors` after the call to `lower_file`
            // and we never use the `ctx` after the call to `lower_file`
            errors: unsafe { std::mem::take(&mut *env.errors.as_ptr()) },
        })?
    }
}

pub fn lower_to_abstr(file: beml_tree::concr::File) -> miette::Result<beml_tree::abstr::File> {
    let mut ctx = lowering::LoweringCtx::new(file.source);
    let mut declarations = im_rc::HashMap::default();
    let terms = file
        .terms
        .into_iter()
        .map(|decl| lowering::rules::decl::lower_decl(&mut ctx, decl))
        .collect::<miette::Result<Vec<_>>>()?;
    for lowering::rules::decl::Defer(f) in terms {
        let decl = f(&mut ctx)?;

        declarations.insert(decl.name().name.clone(), decl);
    }

    if ctx.errors.borrow().is_empty() {
        Ok(beml_tree::abstr::File {
            shebang: file.shebang,
            declarations,
            source: ctx.data,
        })
    } else {
        Err(StepFailedError {
            compiler_pass: CompilerPass::Lowering,

            // this is safe, because we never use the `ctx.errors` after the call to `lower_file`
            // and we never use the `ctx` after the call to `lower_file`
            errors: unsafe { std::mem::take(&mut *ctx.errors.as_ptr()) },
        })?
    }
}
