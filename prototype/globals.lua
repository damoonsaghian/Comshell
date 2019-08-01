local lgi = require 'lgi'
Glib = lgi.GLib
Gio = lgi.Gio
Gdk = lgi.Gdk
Gtk = lgi.Gtk
Editor = lgi.GtkSource
Webkit = lgi.WebKit2

function class(new_class)
  new_class.__index = new_class

  local mt = {
    __call = function(obj)
      setmetatable(obj, new_class)
      return obj
    end
  }
  setmetatable(new_class, mt)

  return new_class
end
