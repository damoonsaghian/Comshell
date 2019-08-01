require 'globals'

local ProjectsList = class {
  paths = {}, -- list of strings
  model = Gtk.ListStore(),
  view = Gtk.TreeView(),

  go_to_project = function(self, project_path)
  end
}

local normal_mode = true

require 'project'
local open_projects = {} -- { "project_name" = Project }
local projects = {};
local main_view = Gtk.Stack();

-- show projects list
local statusbar_message = Gtk.Label {
  label = "",
  single_line_mode = true,
  halign = Gtk.Align.Start,
  margin_start = 2,
  margin_end =2
}
local statusbar_info = Gtk.Label {
  label = "",
  single_line_mode = true,
  halign = Gtk.Align.End,
  margin_start = 2,
  margin_end = 2
}

-- update the date shown in statusbar_info, every (full) minute;
-- (Gtk.Label, Bool) -> Gtk.Continue
function update_date(statusbar_info , redo)
  local now -- =

  if redo then
    Gtk.timeout_add_seconds((60 - now.second()), function() update_date(statusbar_info, true) end)
  end

  local date -- "%F %a %p %I:%M"
  statusbar_info.set_text(date)

  Gtk.Continue(false)
end
updateDateTime(statusbar_info, true)
-- update date after computer wakes up from sleep;
-- https://askubuntu.com/questions/183516/how-do-i-detect-when-my-system-wakes-up-from-suspend-via-dbus-or-similar-in-a-py
-- https://bbs.archlinux.org/viewtopic.php?id=238749

-- this is only for testing;
local view = Webkit.WebView {}
view.load_uri("http://www.google.com/")
main_view.add_named(view, "webview")
-- now connect the widgets, through intermidiate containers;
do
  local statusbar = Gtk.HBox()
  statusbar:pack_start(statusbar_message, true, true, 0)
  statusbar:pack_start(status_bar_info, false, false, 0)
  local root_box = Gtk.VBox()
  root_box:pack_end(statusbar, false, false, 0)
  root_box:pack_end(Gtk.Separator(Gtk.Orientation.HORIZONTAL), false, false, 0)
  root_box:pack_end(main_view, true, true, 0)
  local window = Gtk.Window {
    on_destroy = Gtk.main_quit
  }
  window:add(root_box)
  window:show_all()
  window:maximize()
end

Gtk.main()

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
