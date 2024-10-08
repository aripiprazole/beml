use std::path::PathBuf;

use beml_tree::loc::Source;
use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Args {
    pub main: String,
}

/// The main entry point for the application.
pub fn program() -> miette::Result<()> {
    let args = Args::parse();
    let file = Source::try_from(PathBuf::from(args.main))?;
    let file = beml_syntax::parse_file(file)?;
    let file = beml_analysis::lower_to_abstr(file)?;
    let file = beml_analysis::lower_to_hir(file)?;
    let _ = file;
    Ok(())
}

// The main function wrapper around [`beml::program`].
fn main() {
    unsafe { backtrace_on_stack_overflow::enable() };

    bupropion::install(|| {
        // Build the bupropion handler options, for specific
        // error presenting.
        bupropion::BupropionHandlerOpts::new().width(240)
    })
    .expect("failed to install bupropion handler");

    // Avoid printing print `Error: ` before the error message
    // to maintain the language beauty!
    if let Err(e) = program() {
        println!("{e:?}");
        std::process::exit(1);
    }
}
