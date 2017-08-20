/*
this build script is for linking to MSYS2 libs, for pc-windows-gnu target.

an alternative way would be to add these to .cargo/config:

[target.x86_64-pc-windows-gnu.clutter]
rustc-link-search = ["c:\\msys64\\mingw64\\lib"]
rustc-link-lib = ["evas"]

[target.i686-pc-windows-gnu.clutter]
rustc-link-search = ["c:\\msys32\\mingw32\\lib"]
rustc-link-lib = ["evas"]
*/

use std::path::PathBuf;
use std::env;

fn main(){
  let target = env::var("TARGET").unwrap();
  if target.contains("pc-windows-gnu") {
    let efl_lib_dir = PathBuf::from(
      if target.contains("x86_64") {r"C:\msys64\lib"} else {r"C:\msys32\lib"}
    );
    println!("cargo:rustc-link-search=all={}", efl_lib_dir.display());
  }
}