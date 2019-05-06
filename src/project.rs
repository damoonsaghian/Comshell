use std::collections::HashMap;

use std::{rc::Rc, cell::RefCell};
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
  item: RefCell<Item>,
  file_tree: FilesTree,
  web_browsers: RefCell<HashMap<String, WebBrowser>>,
  external_projects: RefCell<HashMap<String, Project>>
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
  project_views: RefCell<HashMap<String, ProjectView>>,
  notebook: gtk::NoteBook
}

impl Project {
  pub fn go_to_chapter(self, chapter_path: &str) {}
  pub fn prev_chapter(self) {}
  pub fn next_chapter(self) {}
}
