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

-- show projects list
local statusbar_message = gtk.Label {
  label = "",
  single_line_mode = true,
  halign = gtk.Align.Start,
  margin_start = 2,
  margin_end =2
}
local statusbar_info = gtk.Label {
  label = "",
  single_line_mode = true,
  halign = gtk.Align.End,
  margin_start = 2,
  margin_end = 2
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
  local statusbar = gtk.HBox()
  statusbar:pack_start(statusbar_message, true, true, 0)
  statusbar:pack_start(status_bar_info, false, false, 0)
  local root_box = gtk.VBox()
  root_box:pack_end(statusbar, false, false, 0)
  root_box:pack_end(gtk.Separator(gtk.Orientation.HORIZONTAL), false, false, 0)
  root_box:pack_end(main_view, true, true, 0)
  local window = gtk.Window()
  window.on_destroy:connect(gtk.main_quit)
  window:add(root_box)
  window:show_all()
  window:fullscreen()
end

gtk.main()

-[[
# mount drives when a new block device is created;
# stale mount points are automatically removed by "udisksd";
pathtoname() {
  udevadm info -p /sys/"$1" | awk -v FS== '/DEVNAME/ {print $2}'
}
stdbuf -oL -- udevadm monitor --udev -s block | while read -r -- _ _ event devpath _; do
  if [ "$event" = add ]; then
    devname=$(pathtoname "$devpath")
    udisksctl mount --block-device "$devname" --no-user-interaction
  fi
done&
-]]
