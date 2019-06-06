use std::collections::HashMap;

use std::{rc::Rc, cell::RefCell};
use gtk::{self, prelude::*};
use gdk::enums::key;
use sourceview::{self as editor, prelude::*};

pub struct Editor {
  buffer: editor::Buffer,
  view: editor::View
}

impl Editor {
  pub fn new(path: ) -> Self {}
  pub fn prev_page(self) {}
  pub fn next_page(self) {}
  pub fn prev_item(self) {}
  pub fn next_item(self) {}
}

// https://crates.io/crates/gspell
