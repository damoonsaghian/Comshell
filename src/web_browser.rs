use std::collections::HashMap;
use std::{rc::Rc, cell::RefCell};
use gtk::{self, prelude::*};
use gdk::enums::key;
use webkit2gtk::{self as webkit, WebViewExt, WebContextExt};

// "https://crates.io/crates/webkit2gtk"
// "https://github.com/luakit/luakit/tree/develop/lib"
// pdf.js

pub struct WebBrowser {}
