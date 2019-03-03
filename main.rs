use std::collections::HashMap;
use may_actor::Actor;

use std::{rc::Rc, cell::RefCell};
use gtk::{self, prelude::*};
use gdk::enums::key;
use sourceview::{self as editor, prelude::*};
use webkit2gtk::{self as webkit, WebViewExt, WebContextExt};

pub struct TextEditor {
  buffer: editor::Buffer,
  view: editor::View
}

impl TextEditor {
  pub fn prev_page(self) {}
  pub fn next_page(self) {}
  pub fn prev_item(self) {} 
  pub fn next_item(self) {} 
}

pub struct Gallery {}

pub enum Chapter {
  TextEditor,
  Gallery
}

pub struct ProjectTree {}

pub struct ProjectView {
  overlay: ,
  project_tree: ProjectTree,
  chapter: RefCell<Chapter>
}

impl ProjectView {
  fn new() {
    let overlay = ;
    let project_tree = ;
    let vbox = gtk::Box::new();
    vbox.pack();
    ProjectView { overlay, project_tree,  }
  }
}

pub struct Project {
  text_buffers: RefCell<HasMap<String, editor::Buffer>>,
  project_views: RefCell<HashMap<String, ProjectView>>
} 

impl Project {
  pub fn go_to_chapter(self, chapter_path: &str) {}
  pub fn prev_chapter(self) {}
  pub fn next_chapter(self) {}
}

// list of projects in "~/projects/";
pub struct ProjectsList {
  paths_list: RefCell<Vec<Path>>,
  list_view:
}

impl ProjectList {
  pub fn new() -> ProjectList {
    ProjectList {}
  }

  pub fn go_to_project(self, project_path: &str) {}
}

mod r {
  pub struct Refs {
    main_view: gtk::Stack,
    open_projects: HashMap<String, Project>
  }

  ::gtk_fnonce_on_eventloop::with_gtk!(Refs);
  // this macro emits the following public elements:
  //   pub fn init_storage(&Refs);
  //   pub fn do_in_gtk_eventloop( FnOnce(Rc<Refs>) );
}

fn main() {
  if gtk::init().is_err() {
    println!("failed to initialize GTK;");
    return;
  }

  let normal_mode = true;
  let open_projects: Rc<Refcell<HashMap<String, Project>>> =
    Rc::new(RefCell::new(HashMap::new()));
  let projects_list = ProjectsList::new();
  let main_view = gtk::Stack::new();

  // show projects list

  let statusbar_message = gtk::Label::new("");
  statusbar_message.set_single_line_mode(true);
  statusbar_message.set_xalign(0);
  statusbar_message.set_margin_start(2);
  statusbar_message.set_margin_end(2);

  let statusbar_info = gtk::Label::new("");
  statusbar_info.set_single_line_mode(true);
  statusbar_info.set_xalign(1);
  statusbar_info.set_margin_start(2);
  statusbar_info.set_margin_end(2);

  // update the date shown in statusbar_info, every (full) minute;
  ::timer::Timer.new().schedule(::chrono::Local::now(), Some(::std::time::Duration.new(60, 0)),
    move || {
      use chrono::prelude::*;
      let now = Utc::now().with_timezone(&::chrono_tz::...)
      let (is_pm, hour) = now.hour12();
      let date = format!("{year}-{month:02}-{day:02} {weekday:?} {hour:02}:{minute:02}{am_pm}",
                         year = now.year(), month = now.month(), day = now.day(), weekday = now.weekday(),
                         hour = hour, minute = now.minute(), am_pm = if is_pm {"pm"} else {"am"});
      // let date = now.format("%F %a %I:%M%P").to_string();
      r::do_in-gtk_eventloop(|refs| refs.statusbar_info.set_text(&date));
    }
  );

  // this is only for testing;
  let view = webkit::WebView::new();
  view.load_uri("http://www.google.com/");
  main_view.add_named(view, "webview");

  // now connect the widgets, through intermidiate containers;
  {
    let statusbar = gtk::Box::new(gtk::Orientation::HORIZONTAL, 1);
    statusbar.pack_start(statusbar_message, true, true, 0);
    statusbar.pack_start(status_bar_info, false, false, 0);
    let root_box = gtk::Box::new(gtk::Orientation::VERTICAL, 1);
    root_box.pack_end(statusbar, false, false, 0);
    root_box.pack_end(gtk::Separator::new(gtk::Orientation::HORIZONTAL), false, false, 0);
    root_box.pack_end(main_view, true, true, 0);
    let window = gtk::Window::new(gtk::WindowType::Toplevel);
    window.connect_delete_event(move |_, _| {
      gtk::main_quit();
      Inhibit(false)
    });
    window.add(root_box);
    window.show_all();
    window.maximize();
  }

  // and in other threads:
  // r::do_in_gtk_eventloop(|refs| {refs.open_projects.borrow_mut().insert(_, _)});

  r::init_storage(r::Refs {
    main_view,
    open_projects
  });

  gtk::main();
}
