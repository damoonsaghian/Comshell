local lgi = require 'lgi'
local Gio, Gdk, Gtk, GtkSource = lgi.Gio, lgi.Gdk, lgi.Gtk, lgi.GtkSource


local display = Gdk.Display.get_default()
--[[ todo: multi screen
inside each screen create a Comshell window.
opened tabs, text buffers (and webview buffer?), and watched directories are shared between windows.
--]]

local function new_comshell_window()
  -- the main workspace area
  local workspace = Gtk.Stack()
  
  local status_bar = Gtk.Statusbar()
  local ctx = status_bar:get_context_id('default')
  window.status_bar:push(ctx, 'This is statusbar message.')
  
  local vbox = Gtk.VBox()
  vbox:pack_start(workspace, true, true, 0)
  vbox:pack_end(status_bar, false, false, 0)
  
  local window = Gtk.Window {
    on_destroy = Gtk.main_quit, -- on_delete_event = Gtk.main_quit
    -- set windows size to fill the screen
    geometry = display:get_monitor(1).get_geometry(),
    Gtk.Vbox()
  }
  window:maximize() -- to have a proper window, if there is a window manager
  window:show_all()
  
  return window
end


Gtk.main()

-- https://wiki.archlinux.org/index.php/Udisks#udevadm_monitor
-- https://github.com/zserge/luash
