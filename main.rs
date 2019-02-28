use std::collections::HashMap;
use may_actor::Actor;

use gtk;
use gtk::prelude::*;
use gdk::enums::key;

use sourceview as editor;
use sourceview::prelude::*;
use webkit2gtk as webkit;
use webkit2gtk::{WebViewExt, WebContextExt};

#[drive(Clone)]
pub struct TextEditor {
  buffer: editor::Buffer,
  view: editor::View,
}

impl TextEditor {
  pub fn prev_page() {}
  pub fn next_page() {}
  pub fn prev_item() {} 
  pub fn next_item() {} 
}

#[drive(Clone)]
pub struct Gallery {}

#[drive(Clone)]
pub enum Chapter {
  TextEditor,
  Gallery,
}

#[derive(Clone)]
pub struct ProjectTree {}

#[derive(Clone)]
pub struct Project {
  project_tree: ProjectTree,
  open_chapters: HashMap<String, Chapter>,
} 

impl Project {
  pub fn go_to_chapter(chapter_path: &str) {}
  pub fn prev_chapter() {}
  pub fn next_chapter() {}
}

// list of projects in "~/projects/";
#[derive(Clone)]
pub struct ProjectsList {}

impl ProjectList {
  pub fn new() -> ProjectList {
    ProjectList {}
  }

  pub fn go_to_project(&mut self, project_path: &str) {}
}

fn main() {
  if gtk::init().is_err() {
    println!("failed to initialize GTK;");
    return;
  }

  let normal_mode = true;
  let open_projects: HashMap<String, Project> = HashMap::new();
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

  // find a way to update the date shown in statusbar_info, every (full) minute;
  // https://docs.rs/timer/0.2.0/timer/struct.Timer.html#method.schedule
  // eg create a gobject "WallClock" which every (full) minute emits a "notify::clock" signal;
  /*
  WallClock::new().connect("notify::clock", {
    let statusbar_info = statusbar_info.clone();
    move || {
      use chrono::prelude::*;
      // use chrono_tz as tz;
      // let now = Utc::now().with_timezone(&tz::...)
      let now = Local::now();
      let (is_pm, hour) = now.hour12();
      let date = format!("{year}-{month:02}-{day:02} {weekday:?} {hour:02}:{minute:02}{am_pm}",
                         year = now.year(), month = now.month(), day = now.day(), weekday = now.weekday(),
                         hour = hour, minute = now.minute(), am_pm = if is_pm {"pm"} else {"am"});
      // let date = Local::now().format("%F %a %I:%M%P").to_string();
      statusbar_info.set_text(&date);
    }
  });
  */

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
  
  gtk::main();
}
