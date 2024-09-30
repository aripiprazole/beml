#![feature(box_patterns)]
#![feature(new_range_api)]
#![feature(mem_copy_fn)]

pub mod abstr;
pub mod aux;
pub mod concr;
pub mod errors;
pub mod hir;
pub mod lexer;
pub mod loc;
pub mod parser;

use std::path::PathBuf;

use clap::Parser;
use miette::IntoDiagnostic;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Args {
    pub main: String,
}

/// The main entry point for the application.
pub fn program() -> miette::Result<()> {
    bupropion::install(|| {
        // Build the bupropion handler options, for specific
        // error presenting.
        bupropion::BupropionHandlerOpts::new()
    })
    .into_diagnostic()?;
    let args = Args::parse();
    let file = PathBuf::from(args.main);
    let file = parser::parse_file(file)?;
    let file = concr::lower_file(file)?;
    let file = abstr::lower_file(file)?;
    let _ = file;
    Ok(())
}
