use std::path::PathBuf;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Args {
    pub main: String,
}

fn main() -> miette::Result<()> {
    let args = Args::parse();
    let file = PathBuf::from(args.main);
    let file = beml::parser::parse_file(file)?;
    let file = beml::concr::lower_file(file)?;
    let file = beml::abstr::lower_file(file)?;
    let _ = file;
    Ok(())
}
