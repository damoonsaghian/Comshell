const { Gio, Gdk, Gtk } = imports.gi;

Gtk.init(null);

class ProjectsList {
  // [String], Gtk.ListStore, Gtk.TreeView
  constructor(paths, model, view) {}

  go_to_project(project_path) {}
}

let normal_mode = true;

const Project = imports.project.Project;
const open_projects = {}; // { "project name": Project }
const projects = {};
const main_view = new Gtk.Stack();

// show projects list
const statusbar_message = new Gtk.Label({
  label: "",
  single_line_mode: true,
  halign: Gtk.Align.Start,
  margin_start: 2,
  margin_end: 2
});
const statusbar_info = new Gtk.Label({
  label: "",
  single_line_mode: true,
  halign: Gtk.Align.End,
  margin_start: 2,
  margin_end: 2
});

const GnomeDesktop = imports.gi.GnomeDesktop;
new GnomeDesktop.WallClock().connect("notify:clock", () => {
  const GLib = imports.gi.Glib;
  const date = Glib.DateTime.new_now_local().format("%F %a %p %I:%M");
  statusbar_info.set_text(date);
});

// this is only for testing;
const Webkit = imports.gi.WebKit2;
const view = new Webkit.WebView();
view.load_uri("http://www.google.com/");
main_view.add_named(view, "webview");

// now connect the widgets, through intermidiate containers;
{
  const statusbar = new Gtk.HBox();
  statusbar.pack_start(statusbar_message, true, true, 0);
  statusbar.pack_start(status_bar_info, false, false, 0);
  const root_box = new Gtk.VBox();
  root_box.pack_end(statusbar, false, false, 0);
  root_box.pack_end(new Gtk.Separator(Gtk.Orientation.HORIZONTAL), false, false, 0);
  root_box.pack_end(main_view, true, true, 0);
  const window = new Gtk.Window();
  window.connect("destroy", Gtk.main_quit);
  window.add(root_box);
  window.show_all();
  window.maximize();
}

gtk.main();
