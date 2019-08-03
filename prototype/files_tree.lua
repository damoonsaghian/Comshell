-- https://github.com/jonathanBieler/GtkIDE.jl/blob/master/src/sidepanels/FilesPanel.jl
-- https://gitlab.gnome.org/GNOME/shotwell/blob/master/src/sidebar/Tree.vala
-- https://github.com/teejee2008/polo/blob/master/src/Gtk/FileViewList.vala

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

FilesTree = class()
