'use strict';

const main = imports.ui.main;
const st = imports.gi.St;
const clutter = imports.gi.Clutter;
const meta = imports.gi.Meta;

function init() {
  main.setThemeStylesheet(
    "/usr/local/share/gnome-shell/extensions/gnome-shell-improved/style.css");
  main.loadTheme();

  // maximize main windows;
  global.display.connect('window-created', (_display, win) => {
    if (win.can_maximize()) {
      win.maximize(meta.MaximizeFlags.HORIZONTAL | meta.MaximizeFlags.VERTICAL)
    } else {
      win.unmaximize(meta.MaximizeFlags.HORIZONTAL | meta.MaximizeFlags.VERTICAL)
    }
  });

  // move notification banners to the bottom;
  main.messageTray._bannerBin.set_y_align(clutter.ActorAlign.END);

  // move status_bar to the bottom;
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
  main.panel.statusArea.dwellClick.destroy();
  main.panel.statusArea.a11y.destroy();
  main.panel.statusArea.keyboard.destroy();
  main.panel.statusArea.aggregateMenu.destroy();

  // right side of status_bar;
  {
    const rightBox = new st.BoxLayout({ style_class: 'panel-status-indicators-box' });
    main.panel.addToStatusArea("status_right", rightBox, 0, "right");

    // install ArchLinux updates as scheduled;
    // a red indicator appears to notify the user that for system to update, it needs a reboot;

    // an indicator which shows that there are some removable disks which are mounted;

    const status = imports.ui.status;
    leftBox.add_child(status.screencast.Indicator());
    rightBox.add_child(status.location.Indicator());
    leftBox.add_child(status.remoteAccess.RemoteAccessApplet());
    leftBox.add_child(status.rfkill.Indicator());

    const dateTimeIndicator = new st.Label({ y_align: clutter.ActorAlign.CENTER });
    const wallClock = new imports.gi.GnomeDesktop.WallClock();
    wallClock.connect('notify::clock', function() {
      const now = imports.gi.GLib.DateTime.new_now_local();
      const now_formated = now ? now.format("%F %a %p %I:%M") : "";
      // https://github.com/omid/Persian-Calendar-for-Gnome-Shell/blob/master/PersianCalendar%40oxygenws.com/PersianDate.js
      dateTimeIndicator.set_text(now_formated);
    });
    rightBox.add_child(dateTimeIndicator);
  }

  // left side of status_bar;
  {
    const leftBox = new st.BoxLayout({ style_class: "panel-status-indicators-box" });
    main.panel.addToStatusArea("status_left", leftBox, 0, "left");

    const status = imports.ui.status;
    leftBox.add_child(status.network.NMApplet());
    leftBox.add_child(status.volume.Indicator());

    const batteryIndicator = status.power.Indicator();
    // over write "_sync" method, to hide the power icon, if there's no battery;
    batteryIndicator._sync = function() {
      status.power.Indicator.prototype._sync.call(this);
      if (!this._proxy.IsPresent) this.hide();
    };
    batteryIndicator._sync();
    leftBox.add_child(batteryIndicator);

    // https://github.com/hedayaty/NetSpeed
    // https://github.com/Ory0n/Resource_Monitor/
    // https://github.com/corecoding/Vitals
    // https://github.com/paradoxxxzero/gnome-shell-system-monitor-applet
  }

  {
    // little circles in the center of the panel, if there are multiple windows;
    // terminals workspace is special;
    // when a terminal window appears, it will creat a window_group;
    // when other windows appear, they will be added to
    //   the currently active window_group (at index 0);

    // https://gitlab.gnome.org/GNOME/gnome-shell-extensions/tree/master/extensions/window-list

    // list of window_groups in the "terminals" workspace;
    const winGroupList = [];

    global.display.connect('window-created', (_display, win) => {
      if (win.get_workspace() == "terminals") {
        const winGroup = getActiveWinGroup(workspace("terminals"));
        winGroup.addToStart(win);
      }
    });
  }

  // "alt-tab": toggle between "atom" and "browser" workspaces,
  //   go to "atom" workspace from other workspaces,
  //   and launch Atom and the browser, if they are not launched already;

  // "alt-'": go to "terminals" workspace,
  //   and if there is no windows in the "terminals" workspace, open a terminal window;
  // if we are already inside "terminals" workspace, create a new terminal window;

  // "alt-a"/"alt-s": move to left/right window;
  // "alt-escape": close window;

  // "alt-f1": lock the session using "light-locker-command -l";
  // "alt+shift+escape": poweroff/reboot/logout dialog;
  main.wm.addKeybinding();
}

// go to workspace "atom" and launch Atom editor;

/*
https://wiki.gnome.org/Projects/GnomeShell/Extensions/Writing
https://gitlab.gnome.org/GNOME/gnome-shell/tree/master/js/ui
https://gitlab.gnome.org/GNOME/gnome-shell-extensions/tree/master/extensions
*/
