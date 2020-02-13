'use strict';

/*
https://wiki.gnome.org/Projects/GnomeShell/Extensions/Writing
https://github.com/omid/Persian-Calendar-for-Gnome-Shell
https://extensions.gnome.org/extension/1010/archlinux-updates-indicator/
https://gitlab.gnome.org/GNOME/gnome-shell-extensions/tree/master/extensions
https://github.com/hedayaty/NetSpeed
https://github.com/Ory0n/Resource_Monitor/
https://github.com/corecoding/Vitals
https://github.com/paradoxxxzero/gnome-shell-system-monitor-applet
https://extensions.gnome.org/extension/1509/drop-down-terminal-x/
https://extensions.gnome.org/extension/442/drop-down-terminal/
*/

// light-locker-command -l

const gio = imports.gi.Gio;
const st = imports.gi.St;

const main = imports.ui.main;
const panelBox = main.layoutManager.panelBox;
const meta = imports.gi.Meta;

function init() {
  global.display.connect('window-created', (_display, win) => {
    if (win.can_maximize()) {
      win.maximize(meta.MaximizeFlags.HORIZONTAL | meta.MaximizeFlags.VERTICAL)
    } else {
      win.unmaximize(meta.MaximizeFlags.HORIZONTAL | meta.MaximizeFlags.VERTICAL)
    }
  });

  // remove rounded corners;
  main.panel._leftCorner.actor.set_style("-panel-corner-radius: 0px");
  main.panel._rightCorner.actor.set_style("-panel-corner-radius: 0px");

  function movePanelToBottom() {
    const monitor = main.layoutManager.primaryMonitor;
    if (this.rightPanelBarrier) {
      this.rightPanelBarrier.destroy();
    }
    this.rightPanelBarrier = new meta.Barrier({
      display: global.display,
      x1: monitor.width,
      x2: monitor.width,
      y1: monitor.height - panelBox.height,
      y2: monitor.height,
      directions: meta.BarrierDirection.NEGATIVE_Y
    });
    // TODO: find a way to replace the rightPanelBarrier instead of destroying
    main.layoutManager._rightPanelBarrier.destroy();
    panelBox.set_anchor_point(0, (-1)*(monitor.height - panelBox.height));
  }
  main.layoutManager.connect("monitors-changed", movePanelToBottom);
  panelBox.connect("notify::height", movePanelToBottom);
  movePanelToBottom();
  main.panel.actor.add_style_class_name("popup-menu");

  // destroy dateMenu and appMenu;

  // date and time
  dateTime = new St.Label({ y_align: Clutter.ActorAlign.CENTER });
  const wallClock = new imports.gi.GnomeDesktop.WallClock();
  wallClock.connect('notify::clock', function() {
    const
    dateTime.set_text();
  });
  // remove arrow icon;
  main.ui.panel._indicators.get_children().forEach(child => {
    if (child.has_style_class_name("popup-menu-arrow"))
      main.ui.panel._indicators.remove_child(child);
  });
  main.ui.panel._indicators.add_child(dateTime);

  // dots in the middle of panel, if there are multiple windows;

  let notifications = new st.Label();
  main.panel.addToStatusArea('notifications', notifications, 0, 'left');
}
