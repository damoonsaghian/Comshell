use std::collections::HashMap;
use may_actor::Actor;

use std::{rc::Rc, cell::RefCell};
use utils::do_in_main_thread;
use gtk::{self, prelude::*};
use gdk::enums::key;

// list of projects in "~/projects/";
pub struct ProjectsList {
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
  let open_projects: Actor<HashMap<String, Project>> =
    Actor::new(HashMap::new());
  let projects_list = ProjectsList::new();
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

  // update the date shown in statusbar_info, every (full) minute;
  ::timer::Timer.new().schedule(::chrono::Local::now(), Some(::std::time::Duration.new(60, 0)),
    move || {
      use chrono::prelude::*;
      let now = Local::now();
      let (is_pm, hour) = now.hour12();
      let date = format!("{year}-{month:02}-{day:02} {weekday:?} {hour:02}:{minute:02}{am_pm}",
        year = now.year(), month = now.month(), day = now.day(), weekday = now.weekday(),
        hour = hour, minute = now.minute(), am_pm = if is_pm {"pm"} else {"am"});
      // let date = now.format("%F %a %I:%M%P").to_string();
      let statusbar_info_clone = statusbar_info.clone();
      do_in_main_thread(move || {
        statusbar_info.set_text(&date);
      });
    }
  );

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

  // and in other threads:
  // do_in_main_thread(move || { open_projects.borrow_mut().insert(_, _); });

  gtk::main();
}
