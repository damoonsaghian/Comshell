use std::collections::HashMap;
use may_actor;
use gtk;
use gtk::prelude::*; // this imports gtk traits; apparently to use their methods, this is necessary;
use gdk::enums::key;
use sourceview;
use webkit2gtk as webkit;


pub struct ProjectsList {
  pub workspaces: Vec<>,
}

pub struct Project {
  pub chapters_list: ChaptersList,
  pub chapters: Vec<Chapter>,
  widget: gtk::,
} 

/*
go_to_chapter(chapter)
prev_chapter()
next_chapter()
go_to_page(page)
prev_page()
next_page()
prev_item()
next_item()
*/

pub struct Comshell {
  pub projects_list: ProjectsList, 
  pub projects: HashMap<String, Project>,
  current_project_path: String,
  projects_list_is_shown: bool,
  normal_mode: bool,
  main_view: gtk::Stack;
  statusbar_message: gtk::Label,
  statusbar_info: gtk::Label,
  window: gtk::Window,
}

impl Comshell {
  pub fn new() -> Comshell {
    let projects_list = ProjectList::new();
    let projects: Vec<Project> = vec![];

    let main_view = gtk::Stack::new();

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
      let statusbar_info_clone = statusbar_info.clone();
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
        statusbar_info_clone.set_text(&date);
      }
    });
    */

    let window = gtk::Window::new(gtk::WindowType::Toplevel);
    window.connect_delete_event(move |_, _| {
      main_quit();
      Inhibit(false)
    });

    // now connect the widgets;
    {
      let status_bar = gtk::Box::new(gtk::Orientation::HORIZONTAL, 1);
      statusbar.pack_start(statusbar_message, true, true, 0);
      status_bar.pack_start(status_bar_info, false, false, 0);
      let main_box = gtk::Box::new(gtk::Orientation::VERTICAL, 1);
      main_box.pack_end(status_bar, false, false, 0);
      main_box.pack_end(gtk::Separator::new(gtk::Orientation::HORIZONTAL), false, false, 0);
      main_box.pack_end(main_view, true, true, 0);
      window.add(main_box);
    }

    // this is only for testing;
    let view = webkit::WebView::new();
    view.load_uri("http://www.google.com/");
    main_view.add_named(view, "webview");

    Comshell { projects_list , projects, main_view, statusbar_message, statusbar_info, window }
  }

  pub fn window(&self) -> gtk::Window {
    self.window.clone();
  }

  pub fn show_projects_list(&mut self) {}
  pub fn go_to_project(&mut self, project_path: &str) {}
}

fn main() {
  if gtk::init().is_err() {
    eprintln!("failed to initialize GTK;");
    return;
  }

  let comshell = Comshell::new();
  let window = comshell.window();
  window.show_all();
  window.maximize();
  gtk::main();
}
