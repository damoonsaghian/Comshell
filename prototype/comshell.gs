let { GLib, Gio, Gdk, Gtk, Webkit2 } = imports.gi

class ProjectsList
  // [String], Gtk.ListStore, Gtk.TreeView
  def constructor(@paths, @model, @view)

  def go_to_project(project_path)

let mutable normal_mode = true

let { Project } = imports.project
let open_projects = {} // { "project_name": Project }
let projects = {}
let main_view = Gtk.Stack.new()

// show projects list
let statusbar_message = Gtk.Label.new {
  label: "",
  single_line_mode: true,
  halign: Gtk.Align.Start,
  margin_start: 2,
  margin_end: 2
}
let statusbar_info = Gtk.Label.new {
  label: "",
  single_line_mode: true,
  halign: Gtk.Align.End,
  margin_start: 2,
  margin_end: 2
}

let { GnomeDesktop } = imports.gi
GnomeDesktop.WallClock.new().connect("notify:clock", #
  let date = Glib.DateTime.new_now_local().format("%F %a %p %I:%M")
  statusbar_info.set_text(date)
)

// this is only for testing;
let view = Webkit.WebView.new {}
view.load_uri("http://www.google.com/")
main_view.add_named(view, "webview")

// now connect the widgets, through intermidiate containers;
(#
  let statusbar = Gtk.HBox.new()
  statusbar.pack_start(statusbar_message, true, true, 0)
  statusbar.pack_start(status_bar_info, false, false, 0)
  let root_box = Gtk.VBox.new()
  root_box.pack_end(statusbar, false, false, 0)
  root_box.pack_end(Gtk.Separator(Gtk.Orientation.HORIZONTAL), false, false, 0)
  root_box.pack_end(main_view, true, true, 0)
  let window = Gtk.Window.new()
  window.connect("destroy", Gtk.main_quit)
  window.add(root_box)
  window.show_all()
  window.maximize()
)()

gtk.main()
