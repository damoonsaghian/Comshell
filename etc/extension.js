'use strict';

/*
https://wiki.gnome.org/Projects/GnomeShell/Extensions/Writing
https://blog.fpmurphy.com/2011/11/updating-gnome-shell-extensions-to-work-with-gnome-3-2.html
https://www.bidon.ca/fr/random/2017-02-25-customizing-gnome-shell-3-20
https://github.com/home-sweet-gnome/dash-to-panel
https://github.com/KEIII/gnome-shell-panel-date-format
https://github.com/Daniel-Khodabakhsh/datetime-format
https://github.com/omid/Persian-Calendar-for-Gnome-Shell
https://extensions.gnome.org/extension/1010/archlinux-updates-indicator/
https://developer.gnome.org/shell/stable/
https://gitlab.gnome.org/GNOME/gnome-shell-extensions/tree/master/extensions/window-list
https://gitlab.gnome.org/GNOME/gnome-shell-extensions/tree/master/extensions/user-theme
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

  let date_time = new st.Label();
  main.panel.addToStatusArea('date_time', date_time);
}
