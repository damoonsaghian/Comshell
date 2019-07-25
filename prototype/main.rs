use std::collections::HashMap;
use std::{rc::Rc, cell::RefCell};
use gtk::{self, prelude::*};
use gdk::enums::key;

pub struct Directory {}

struct ProjectsList {
  paths_list: RefCell<Vec<Path>>,
  model: gtk::ListStore,
  view: gtk::TreeView
}

impl ProjectList {
  pub fn new() -> ProjectList {
  ProjectList {}
  }

  pub fn go_to_project(self, project_path: &str) {}
}

fn main() {
  if gtk::init().is_err() {
    println!("failed to initialize GTK;");
    return;
  }

  let normal_mode = true;
  use project::Project;
  let open_projects: Rc<RefCell<HashMap<String, Project>>> =
    Rc::new(RefCell::new(HashMap::new()));
  let projects_list = Rc::new(ProjectsList::new());
  let main_view = gtk::Stack::new();

  // show projects list

  let statusbar_message = gtk::Label::new("");
  statusbar_message.set_single_line_mode(true);
  statusbar_message.set_halign(gtk::Align::Start);
  statusbar_message.set_margin_start(2);
  statusbar_message.set_margin_end(2);

  let statusbar_info = gtk::Label::new("");
  statusbar_info.set_single_line_mode(true);
  statusbar_info.set_halign(gtk::Align::End);
  statusbar_info.set_margin_start(2);
  statusbar_info.set_margin_end(2);

  let status_bar_info_clone = status_bar_info;
  gtk::timeout_add_seconds(1, move || {
    use chrono::prelude::*;
    let now = Local::now();

    let (is_pm, hour) = now.hour12();
    let date = format!("{year}-{month:02}-{day:02} {weekday:?} {am_pm} {hour:02}:{minute:02}",
      year = now.year(), month = now.month(), day = now.day(), weekday = now.weekday(),
      am_pm = if is_pm {"PM"} else {"AM"}, hour = hour, minute = now.minute());
    // let date = now.format("%F %a %p %I:%M").to_string();
    status_bar_info_clone.set_text(&date);

    gtk::Continue(true)
  });

  // this is only for testing;
  use webkit2gtk::{self as webkit, WebViewExt, WebContextExt};
  let view = webkit::WebView::new();
  view.load_uri("http://www.google.com/");
  main_view.add_named(&view, "webview");

  // now connect the widgets, through intermidiate containers;
  {
    let statusbar = gtk::Box::new(gtk::Orientation::HORIZONTAL, 0);
    statusbar.pack_start(&statusbar_message, true, true, 0);
    statusbar.pack_start(&status_bar_info, false, false, 0);
    let root_box = gtk::Box::new(gtk::Orientation::VERTICAL, 0);
    root_box.pack_end(&statusbar, false, false, 0);
    root_box.pack_end(gtk::Separator::new(gtk::Orientation::HORIZONTAL), false, false, 0);
    root_box.pack_end(&main_view, true, true, 0);
    let window = gtk::Window::new(gtk::WindowType::Toplevel);
    window.connect_delete_event(move |_, _| {
      gtk::main_quit();
      gtk::Inhibit(false)
    });
    window.add(&root_box);
    window.show_all();
    window.maximize();
  }

  gtk::main();
}
