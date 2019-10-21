local lgi = require "lgi"
glib = lgi.GLib
gio = lgi.Gio
gdk = lgi.Gdk
gtk = lgi.Gtk
geditor = lgi.GtkSource
webkit = lgi.WebKit2

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
