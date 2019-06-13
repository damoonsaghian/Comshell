use std::collections::HashMap;
use may_actor::Actor;

use std::{rc::Rc, cell::RefCell};
use main::do_in_main;
use gtk::{self, prelude::*};
use gdk::enums::key;

use gallery::Gallery;
use editor::Editor;

pub enum Item {
  Editor,
  Gallery
}

use files_tree::FilesTree;
use web_browser::WebBrowser;

pub struct View {
  item: Item,
  files_tree: FilesTree,
  floating_layer: gtk::Container
}

impl View {
  fn new() {
    let  = ;
    let tree_view = ;
    let vbox = gtk::Box::new();
    vbox.pack();
    ProjectView { overlay, project_tree,  }
  }
}

pub struct Project {
  views: RefCell<HashMap<String, View>>,
  notebook: gtk::NoteBook,
  web_browsers: RefCell<HashMap<String, WebBrowser>>,
  external_projects: RefCell<HashMap<String, Project>>
}

impl Project {
  pub fn go_to_chapter(self, chapter_path: &str) {}
  pub fn prev_chapter(self) {}
  pub fn next_chapter(self) {}
}
