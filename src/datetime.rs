// update the date shown in statusbar_info, every (full) minute;
fn updateDateTime(statusbar_info: gtk::Label, repeat: bool) -> gtk::Continue {
  use chrono::prelude::*;
  let now = Local::now();

  if repeat {
    gtk::timeout_add_seconds((60 - now.second()), move || updateDateTime(statusbar_info, true));
  }

  let (is_pm, hour) = now.hour12();
  let date = format!("{year}-{month:02}-{day:02} {weekday:?} {am_pm} {hour:02}:{minute:02}",
    year = now.year(), month = now.month(), day = now.day(), weekday = now.weekday(),
    am_pm = if is_pm {"PM"} else {"AM"}, hour = hour, minute = now.minute());
  // let date = now.format("%F %a %p %I:%M").to_string();
  statusbar_info.set_text(&date);

  gtk::Continue(false)
}
updateDateTime(statusbar_info.clone(), true);
/*
// update date after computer wakes up from sleep;
// https://askubuntu.com/questions/183516/how-do-i-detect-when-my-system-wakes-up-from-suspend-via-dbus-or-similar-in-a-py
*/

/*
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

update date after computer wakes up from sleep;
https://askubuntu.com/questions/183516/how-do-i-detect-when-my-system-wakes-up-from-suspend-via-dbus-or-similar-in-a-py
https://bbs.archlinux.org/viewtopic.php?id=238749
*/
