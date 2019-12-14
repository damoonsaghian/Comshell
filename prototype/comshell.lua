require "globals"

local ProjectsList = class()
function ProjectList:init(defaults)
  self.paths = defaults.paths and {} -- list of strings
  self.model = gtk.ListStore()
  self.view = gtk.TreeView()
end

function ProjectList:go_to_project(project_path)
end

local normal_mode = true

require "project"
local open_projects = {} -- { "project_name" = Project }
local projects = {}
local main_view = gtk.Stack()
main_view.set_hexpand(true)
main_view.set_vexpand(true)

-- show projects list
local statusbar_message = gtk.Label {
  label = "",
  single_line_mode = true,
  halign = gtk.Align.Start,
  margin_start = 2,
  margin_end = 2,
  halign = gtk.Align.START
}
local statusbar_info = gtk.Label {
  label = "",
  single_line_mode = true,
  halign = gtk.Align.End,
  margin_start = 2,
  margin_end = 2,
  halign = gtk.Align.END
}

require("lgi").GnomeDesktop.WallClock().on_notify:connect(function()
  local date = glib.DateTime.new_now_local().format("%F %a %p %I:%M")
  statusbar_info:set_text(date)
end, "clock")

--[[
https://www.techrapid.uk/2017/04/automatically-update-arch-linux-with-systemd.html
https://wiki.archlinux.org/index.php/Systemd/Timers
download updates as scheduled;
put "reboot to update" in notifications;
before reboot/poweroff install the updates, then delete the notification file;
--]]

-- this is only for testing;
local view = Webkit.WebView {}
view:load_uri("http://www.google.com/")
main_view:add_named(view, "webview")

-- now connect the widgets, through intermidiate containers;
do
  local statusbar = gtk.Grid {
    orientation = gtk.Orientation.HORIZONTAL,
    statusbar_message,
    statusbar_info
  }
  local root_box = gtk.Grid {
    orientation = gtk.Orientation.VERTICAL,
    main_view,
    gtk.Separator(gtk.Orientation.HORIZONTAL),
    statusbar
  }
  local window = gtk.Window { root_box }
  window.on_destroy:connect(function(_) gtk.main_quit() end)
  window:show_all()
  window:fullscreen()
end

gtk.main()
