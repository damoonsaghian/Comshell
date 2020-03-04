function enable() {
const main = imports.ui.main;
const st = imports.gi.St;
const clutter = imports.gi.Clutter;
const meta = imports.gi.Meta;
const shell = imports.gi.Shell;

main.setThemeStylesheet(
  "/usr/local/share/gnome-shell/extensions/gnome-shell-improved/style.css");
main.loadTheme();

// simplify "overview" such that it just shows the installed apps;
{
  const overview = main.overview;
  const viewSelector = overview.viewSelector;
  // hide "frequent/all" buttons;
  viewSelector.appDisplay._controls.hide();
  // switch to "all apps" view;
  viewSelector.appDisplay._showView(1);

  // show apps instead of windows and workspaces;
  function showApps() {
	  overview._dash.actor.hide();
	  viewSelector.actor.set_x(0);
	  viewSelector.actor.set_width(0);
	  viewSelector.actor.queue_redraw();
    viewSelector.showApps();
  }
  overview.connect("showing", showApps);
}

// open apps in separate workspaces;
shell.App.prototype.activate = function() {
  const appWindows = this.get_windows();
  if (appWindows && appWindows.length > 0) {
    appWindows[0].get_workspace().activate(0);
    return;
  }
  const newWorkspace = global.workspace_manager.append_new_workspace(true, 0);
  this.activate_full(newWorkspace.index(), 0);
};

// when Comshell is installed on the system, it will be launched at startup;
// "alt-tab" on Comshell activates previously visited workspace;
// "alt-tab" on other applications (other than Comshell) will activate Comshell;
// when Atom editor and Firefox browser are installed on the system (and Comshell is not),
//   Atom will be launched at startup instead of Comshell;
// "alt-tab" toggles between Atom and Firefox;
// "alt-tab" on other applications will activate Atom;
// if Comshell or Atom/Firefox is not installes, Termite will be launched at startup,
//   and "alt-tab" activates previously visited workspace;
{
  // a function which gets a "String" and returns a "shell.App" or null;
  const appSystem = shell.AppSystem.get_default();

  const comshellApp = appSystem.lookup_app("comshell.desktop");
  const atomApp = appSystem.lookup_app("atom.desktop");
  const firefoxApp = appSystem.lookup_app("firefox.desktop");
  const termApp = appSystem.lookup_app("termite.desktop");

  if (comshellApp) {
    comshellApp.activate_full(-1, 0);
  } else if (atomApp && firefoxApp) {
    atomApp.activate_full(-1, 0);
  } else if (termApp) {
    termApp.activate_full(-1, 0);
  }

  const tracker = shell.WindowTracker.get_default();

  function altTabHandler(_display, _window, _binding) {
    const comshellApp = appSystem.lookup_app("comshell.desktop");
    const atomApp = appSystem.lookup_app("atom.desktop");
    const firefoxApp = appSystem.lookup_app("firefox.desktop");

    if (comshellApp) {
      const focusedWindow = global.display.get_focus_window();
      const focusedApp = tracker.get_window_app(focusedWindow);
      if (focusedApp == null || focusedApp.is_window_backed()) {
        comshellApp.activate();
        return;
      }
      const focusedAppId = focusedApp.get_id();
      if (focusedAppId === "comshell.desktop") {
        appSystem.get_running()[1].get_windows()[0].getWorkspace().activate(0);
      } else {
        comshellApp.activate();
      }
    } else if (atomApp && firefoxApp) {
      const focusedWindow = global.display.get_focus_window();
      if (!focusedWindow) {
        atomApp.activate();
        return;
      }
      const app = tracker.get_window_app(focusedWindow);
      if (app == null || app.is_window_backed()) {
        atomApp.activate();
        return;
      }
      const appId = app.get_id();
      if (appId === "atom.desktop") {
        firefoxApp.activate();
      } else if (appId === "chromium.desktop") {
        atomApp.activate();
      } else {
        atomApp.activate();
      }
    } else {
      appSystem.get_running()[1].get_windows()[0].getWorkspace().activate(0);
    }
  }

  main.wm.setCustomKeybindingHandler("switch-applications",
    shell.ActionMode.NORMAL,
    altTabHandler
  );

  // activate terminal app; if we are in terminal workspace, open a new terminal window;
  main.wm.removeKeybinding("switch-to-application-1");
  main.wm.addKeybinding("switch-to-application-1",
    new imports.gi.Gio.Settings({ schema_id: imports.ui.windowManager.SHELL_KEYBINDINGS_SCHEMA }),
    meta.KeyBindingFlags.IGNORE_AUTOREPEAT,
    shell.ActionMode.NORMAL | shell.ActionMode.OVERVIEW,
    () => {
      const termWindows = termApp.get_windows();
      if (termWindows && termWindows.length > 0) {
        const termWorkspace = termWindows[0].get_workspace();
        const activeWorkspace = global.workspace_manager.get_active_workspace();
        if (termWorkspace === activeWorkspace) {
          termApp.open_new_window(-1);
          return;
        }
      }
      termApp.activate();
    }
  );
}

// move notification banners to the bottom;
{
  const bannerBin = main.messageTray._bannerBin;
  if (bannerBin) bannerBin.set_y_align(clutter.ActorAlign.END);
}

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
    const rightPanelBarrier = main.layoutManager._rightPanelBarrier;
    if (rightPanelBarrier) rightPanelBarrier.destroy();
    panelBox.set_anchor_point(0, (-1)*(monitor.height - panelBox.height));
  }
  main.layoutManager.connect("monitors-changed", movePanelToBottom);
  panelBox.connect("notify::height", movePanelToBottom);
  movePanelToBottom();
}

main.panel.statusArea.activities.container.hide();
main.panel.statusArea.appMenu.destroy();
main.panel.statusArea.dateMenu.container.hide();
main.panel.statusArea.aggregateMenu.container.hide();

// right side of status_bar;
{
  const rightButton = new imports.ui.panelMenu.Button(0.0, null, true);
  main.panel.addToStatusArea("status_right", rightButton, 0, "right");
  const rightBox = new st.BoxLayout({ style_class: "panel-status-indicators-box" });
  rightButton.add_child(rightBox);

  // install ArchLinux updates as scheduled;
  // a red indicator appears to notify the user that for system to update, it needs a reboot;

  const screencast = main.panel.statusArea.aggregateMenu._screencast;
  if (screencast && screencast._indicator) {
    screencast.indicators.remove_child(screencast._indicator);
    rightBox.add_child(screencast._indicator);
    screencast._sync();
  }
  /*
  // since screencast implementation is simple, an alternative method is:
  const screencastIcon = new st.Icon({ style_class: "system-status-icon" });
  rightBox.add_child(screencastIcon);
  screencastIcon.icon_name = "media-record-symbolic";
  screencastIcon.add_style_class_name("screencast-indicator");
  main.screencastService.connect("updated",
    () => screencastIcon.visible = main.screencastService.isRecording
  );
  */

  const location = main.panel.statusArea.aggregateMenu._location;
  if (location && location._indicator) {
    location.indicators.remove_child(location._indicator);
    rightBox.add_child(location._indicator);
    location._syncIndicator();
  }

  const remoteAccess = main.panel.statusArea.aggregateMenu._remoteAccess;
  if (remoteAccess && remoteAccess._indicator) {
    remoteAccess._ensureControls();
    remoteAccess.indicators.remove_child(remoteAccess._indicator);
    rightBox.add_child(remoteAccess._indicator);
    remoteAccess._sync();
  }

  const dateTimeLabel = new st.Label({ y_align: clutter.ActorAlign.END });
  rightBox.add_child(dateTimeLabel);
  const updateClock = () => {
    const now = imports.gi.GLib.DateTime.new_now_local();
    const nowFormated = now ? now.format("%F %a %p %I:%M") : "";
    // https://github.com/omid/Persian-Calendar-for-Gnome-Shell/blob/master/PersianCalendar%40oxygenws.com/PersianDate.js
    dateTimeLabel.set_text(nowFormated);
  };
  updateClock();
  const wallClock = main.panel.statusArea.dateMenu._clock;
  if (wallClock) wallClock.connect("notify::clock", updateClock);
}

// left side of status_bar;
{
  const leftButton = new imports.ui.panelMenu.Button(0.0, null, true);
  main.panel.addToStatusArea("status_left", leftButton, 0, "left");
  const leftBox = new st.BoxLayout({ style_class: "panel-status-indicators-box" });
  leftButton.add_child(leftBox);

  const network = main.panel.statusArea.aggregateMenu._network;
  if (network && network._primaryIndicator) {
    network.indicators.remove_child(network._primaryIndicator);
    leftBox.add_child(network._primaryIndicator);
    if (network._vpnIndicator) {
      network.indicators.remove_child(network._vpnIndicator);
      leftBox.add_child(network._vpnIndicator);
    }
  }

  // https://github.com/hedayaty/NetSpeed
  // net usage

  const rfkillIcon = new st.Icon({ style_class: "system-status-icon" });
  leftBox.add_child(rfkillIcon);
  rfkillIcon.icon_name = "network-wireless-disabled-symbolic";
  const rfkillManager = imports.ui.status.rfkill.getRfkillManager();
  rfkillIcon.visible = rfkillManager.airplaneMode;
  rfkillManager.connect("airplane-mode-changed",
    () => rfkillIcon.visible = rfkillManager.airplaneMode
  );

  const volume = main.panel.statusArea.aggregateMenu._volume;
  if (volume && volume._primaryIndicator) {
    volume.indicators.remove_child(volume._primaryIndicator);
    leftBox.add_child(volume._primaryIndicator);
    if (volume._inputIndicator) {
      volume.indicators.remove_child(volume._inputIndicator);
      leftBox.add_child(volume._inputIndicator);
      volume._inputIndicator.visible = true;
    }
  }

  const power = main.panel.statusArea.aggregateMenu._power;
  if (power && power._indicator) {
    power.indicators.remove_child(power._indicator);
    leftBox.add_child(power._indicator);
    if (power._percentageLabel) {
      power.indicators.remove_child(power._percentageLabel);
      leftBox.add_child(power._percentageLabel);
    }
    // over_write "_sync" method, to hide the power icon, if there's no battery;
    if (power._sync && power._proxy) {
      power._sync = function() {
        imports.ui.status.power.Indicator.prototype._sync.call(this);
        if (!this._proxy.IsPresent) this._indicator.hide();
      };
      power._sync();
    }
  }

  // https://github.com/Ory0n/Resource_Monitor/
  // https://github.com/corecoding/Vitals
  // https://github.com/paradoxxxzero/gnome-shell-system-monitor-applet
}
}
