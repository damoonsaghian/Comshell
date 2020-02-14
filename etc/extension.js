'use strict';

const main = imports.ui.main;
const st = imports.gi.St;
const meta = imports.gi.Meta;

function init() {
  // maximize main windows;
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

  // move status_bar to bottom;
  {
    const panelBox = main.layoutManager.panelBox;
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
  }

  main.panel.statusArea.activities.destroy();
  main.panel.statusArea.appMenu.destroy();
  main.panel.statusArea.dateMenu.destroy();

  // create a dateTimeIndicator and add it to aggregateMenu;
  {
    const dateTimeIndicator = new st.Label({ y_align: Clutter.ActorAlign.CENTER });
    const DateTime = imports.gi.GLib.DateTime;
    const wallClock = new imports.gi.GnomeDesktop.WallClock();
    wallClock.connect('notify::clock', function() {
      const now = DateTime.new_now_local();
      const now_formated = now ? now.format("%F %a %p %I:%M") : "";
      dateTimeIndicator.set_text(now_formated);
    });

    const aggregateMenu = main.ui.panel.statusArea.aggregateMenu;
    // first remove the power and arrow icons;
    aggregateMenu._indicators.remove_child(aggregateMenu._power);
    aggregateMenu._indicators.get_children().forEach(child => {
      if (child.has_style_class_name("popup-menu-arrow"))
        aggregateMenu._indicators.remove_child(child);
    });
    aggregateMenu._indicators.add_child(dateTimeIndicator);
  }

  // put messageTray at the left of the status_bar;
  main.panel.addToStatusArea('notifications', main.messageTray, 0, 'left');

  // dots in the middle of panel, if there are multiple windows;

  // "alt-f1": lock the session using "light-locker-command -l";
}

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
