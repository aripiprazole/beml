// The main function wrapper around [`beml::program`].
fn main() {
    // Avoid printing print `Error: ` before the error message
    // to maintain the language beauty!
    if let Err(e) = beml::program() {
        eprintln!("{e:?}");
        std::process::exit(1);
    }
}
