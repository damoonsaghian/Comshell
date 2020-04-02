const gtk = imports.gi.Gtk;
const gdk = imports.gi.Gdk;
const webkit = imports.gi.WebKit2;

const window = new gtk.Window();
window.connect("delete-event", gtk.main_quit);
window.add(main_view);
window.show_all();
window.maximize();
gtk.main();
