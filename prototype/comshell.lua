local lgi = require 'lgi'
local gtk = lgi.Gtk
local gdk = lgi.Gdk

local utils = require 'utils'
local class = utils.class

local Directory = class()

local ProjectsList = class()
function ProjectsList:init()
  self.paths = self.paths or [] -- list of strings
  self.model = self.model or gtk.ListStore()
  self.view = self.view gtk.TreeView()
end
function ProjectsList:go_to_project(project_path)
end

local normal_mode = true

local Project = require 'project'
local open_projects = {} -- { "project_name" = Project }
local projects = {};
local main_view = gtk.Stack();

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

-- update the date shown in statusbar_info, every (full) minute;
-- (gtk.Label, Bool) -> gtk.Continue
function update_date(statusbar_info , redo)
  local now -- =

  if redo then
    gtk.timeout_add_seconds((60 - now.second()), function() update_date(statusbar_info, true) end)
  end

  local date -- "%F %a %p %I:%M"
  statusbar_info.set_text(date)

  gtk.Continue(false)
end
updateDateTime(statusbar_info, true)
-- update date after computer wakes up from sleep;
-- https://askubuntu.com/questions/183516/how-do-i-detect-when-my-system-wakes-up-from-suspend-via-dbus-or-similar-in-a-py
-- https://bbs.archlinux.org/viewtopic.php?id=238749

-- this is only for testing;
local webkit = lgi.WebKit2
local view = webkit.WebView {}
view.load_uri("http://www.google.com/")
main_view.add_named(view, "webview")
-- now connect the widgets, through intermidiate containers;
do
  local statusbar = gtk.Box.new(gtk.Orientation.HORIZONTAL, 0)
  statusbar.pack_start(statusbar_message, true, true, 0)
  statusbar.pack_start(status_bar_info, false, false, 0)
  local root_box = gtk.Box.new(gtk.Orientation.VERTICAL, 0)
  root_box.pack_end(statusbar, false, false, 0)
  root_box.pack_end(gtk.Separator.new(gtk.Orientation.HORIZONTAL), false, false, 0)
  root_box.pack_end(main_view, true, true, 0)
  let window = gtk.Window.new(gtk.WindowType.Toplevel)
  window.connect_delete_event(function()
    gtk.main_quit()
    gtk.Inhibit(false)
  end)
  window.add(root_box)
  window.show_all()
  window.maximize()
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
