const GObject = imports.gi.GObject;
const GLib = imports.gi.GLib;
const Gio = imports.gi.Gio;
const Gdk = imports.gi.Gdk;
const Gtk = imports.gi.Gtk;
const GtkSource = imports.gi.GtkSource;
const WebKit = imports.gi.WebKit2;
const WallClock =imports.gi.GnomeDesktop.WallClock;
Gtk.init(null);

let window = Gtk.Window.new(Gtk.WindowType.TOPLEVEL);
window.connect("destroy", Gtk.main_quit);
window.window_position = Gtk.WindowPosition.CENTER

let main_box = Gtk.Box.new(Gtk.Orientation.VERTICAL, 1);
window.add(main_box);

let status_bar = {
  box: Gtk.Box.new(Gtk.Orientation.HORIZONTAL, 1),
  messages: Gtk.Label.new("welcome to Comshell;"),
  system_info: Gtk.Label.new(
    GLib.DateTime.new_now_local().format("%F %a %I:%M%P")
  ),
  wc: WallClock.new()
};
main_box.pack_end(status_bar.box, false, false, 0);

status_bar.messages.single_line_mode = true;
status_bar.messages.xalign = 0;
status_bar.messages.margin_start = 2;
status_bar.messages.margin_end = 2;
status_bar.box.pack_start(status_bar.messages, true, true, 0);
status_bar.system_info.single_line_mode = true;
status_bar.system_info.xalign = 1;
status_bar.system_info.margin_start = 2;
status_bar.system_info.margin_end = 2;
status_bar.box.pack_start(status_bar.system_info, false, false, 0);
status_bar.wc.connect("notify::clock", function() {
  status_bar.system_info.set_text(
    GLib.DateTime.new_now_local().format("%F %a %I:%M%P")
  );
});

main_box.pack_end(Gtk.Separator.new(Gtk.Orientation.HORIZONTAL), false, false, 0);

let views_stack = Gtk.Stack.new();
main_box.pack_end(views_stack, true, true, 0);

let view = WebKit.WebView.new();
view.load_uri("http://www.google.com/");
views_stack.add_named(view, "webview");

window.show_all();
window.maximize();
Gtk.main();
