const gtk = imports.gi.Gtk;
const gdk = imports.gi.Gdk;
const gio = imports.gi.Gio;

class Overview {}

class Editor {}

class FilesTree {}

class Gallery {}

class WebBrowser {}

class Project {}

gtk.init(null);

const open_projects = [];

const main_view = new gtk.Stack();

const over_view = new Overview();

const window = new gtk.Window();
window.connect("delete-event", gtk.main_quit);
window.add(main_view);
window.show_all();
window.maximize();
gtk.main();
