use std::env;

use cbindgen::Config;

fn main() {
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();

    cbindgen::Builder::new()
        .with_crate(crate_dir)
        .with_config(Config::from_file("cbindgen.toml").expect("Unable to read cbindgen.toml"))
        .generate()
        .expect("Unable to generate bindings")
        .write_to_file("include/swcjs_binding.h");
}
