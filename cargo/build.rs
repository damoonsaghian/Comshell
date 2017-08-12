/*
this build script is for linking to EFL libs, compiled and installed using MSYS2. (for pc-windows-gnu target)
note that MSYS2 must be installed in "C:\msys64" or "C:\msys32".

*/

use std::path::PathBuf;
use std::env;

fn main(){
  let target = env::var("TARGET").unwrap();
  if target.contains("pc-windows-gnu") {
    let efl_lib_dir = PathBuf::from(
      if target.contains("x86_64") {r"C:\msys64\usr\local\lib"} else {r"C:\msys32\usr\local\lib"}
    );
    println!("cargo:rustc-link-search=all={}", efl_lib_dir.display());
  }
}