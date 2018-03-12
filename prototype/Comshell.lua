local lgi = require 'lgi'
local Gio, Gdk, Gtk, GtkSource = lgi.Gio, lgi.Gdk, lgi.Gtk, lgi.GtkSource
local Gst, GstVideo = lgi.Gst, lgi.GstVideo


local display = Gdk.Display.get_default()

local function new_comshell_window(monitor_n=1)
  
  local doc = Gtk.TextEditor {
    expand = true,
  }
  
  local tab = Gtk.Box {
    orientation = 'VERTICAL',
    doc
  }
  
  -- the main workspace area
  local workspace = Gtk.Stack()
  
  local window = Gtk.Window {
    on_delete_event = Gtk.main_quit,
    workspace
  }
  -- set windows size to fill the screen
  local rectangle = display:get_monitor(monitor_n).get_geometry()
  window:show_all()
  window:move(rectangle.x, rectangle.y)
  window:resize(rectangle.width, rectangle.height)
  window:maximize() -- to have a proper window, if there is a window manager (with panels ...).
  
  return window
end

--[[ todo: multi screen support
inside each screen create a Comshell window.
move between them with Gtk.Window.present().
opened tabs, text buffers (and webview buffer?), and watched directories are shared between windows.
--]]
new_comshell_window()

Gtk.main()

-- https://wiki.archlinux.org/index.php/Udisks#udevadm_monitor
-- https://github.com/zserge/luash
