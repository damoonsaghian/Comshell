local lgi = require 'lgi'
Glib = lgi.GLib
Gio = lgi.Gio
GnomeDesktop = lgi.GnomeDesktop
Gdk = lgi.Gdk
Gtk = lgi.Gtk
Editor = lgi.GtkSource
Webkit = lgi.WebKit2

function class()
  local new_class = {}
  new_class.__index = new_class
  function new_class:init(_) return {} end

  local mt = {
    __call = function(defaults)
      local defaults = defaults and {}
      local obj = {}
      setmetatable(obj, new_class)
      obj:init(defaults)
      return obj
    end
  }
  setmetatable(new_class, mt)

  return new_class
end
