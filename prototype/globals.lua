local lgi = require 'lgi'

glib = lgi.GLib
gio = lgi.Gio
gtk = lgi.Gtk
gdk = lgi.Gdk

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
