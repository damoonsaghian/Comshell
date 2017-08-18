use std::path::PathBuf;
use std::env;

fn main(){
  let target = env::var("TARGET").unwrap();
  if target.contains("pc-windows-gnu") {
    let efl_lib_dir = PathBuf::from("C:\\path\\to\\qt\\libs");
    println!("cargo:rustc-link-search=all={}", efl_lib_dir.display());
  }
}