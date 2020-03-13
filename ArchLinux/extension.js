function enable() {
const main = imports.ui.main;
const st = imports.gi.St;
const clutter = imports.gi.Clutter;
const meta = imports.gi.Meta;
const shell = imports.gi.Shell;

main.setThemeStylesheet(
  "/usr/local/share/gnome-shell/extensions/gnome-shell-improved/style.css");
main.loadTheme();

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

  const remoteAccess = main.panel.statusArea.aggregateMenu._remoteAccess;
  if (remoteAccess && remoteAccess._indicator) {
    remoteAccess._ensureControls();
    remoteAccess.indicators.remove_child(remoteAccess._indicator);
    rightBox.add_child(remoteAccess._indicator);
    remoteAccess._sync();
  }

  const location = main.panel.statusArea.aggregateMenu._location;
  if (location && location._indicator) {
    location.indicators.remove_child(location._indicator);
    rightBox.add_child(location._indicator);
    location._syncIndicator();
  }

  const dateTimeLabel = new st.Label({ y_align: clutter.ActorAlign.CENTER });
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

{
  const overview = main.overview;
  const viewSelector = overview.viewSelector;
  const windowManager = main.wm;

  // apps view simplified;
  try {
    // switch to "all apps" view;
    viewSelector.appDisplay._showView(1);
    // hide "frequent/all" buttons;
    viewSelector.appDisplay._controls.hide();
  } finally {}

  // apps view with toggle functionality
  windowManager.removeKeybinding("toggle-application-view");
  windowManager.addKeybinding(
    "toggle-application-view",
    new imports.gi.Gio.Settings({ schema_id: imports.ui.windowManager.SHELL_KEYBINDINGS_SCHEMA }),
    meta.KeyBindingFlags.IGNORE_AUTOREPEAT,
    shell.ActionMode.NORMAL | shell.ActionMode.OVERVIEW,
    () => {
      if (overview.isDummy) return;
      if (overview.visible) overview.hide();
      else viewSelector.showApps();
    }
  );

  // hide apps view when pressing "alt-tab";
  windowManager.setCustomKeybindingHandler(
    "switch-applications",
    shell.ActionMode.NORMAL | shell.ActionMode.OVERVIEW,
    (display, win, binding) => {
      if (overview.visible) { overview.hide(); return; }
      windowManager._startSwitcher(display, win, binding);
    }
  );

  const appSystem = shell.AppSystem.get_default();
  const windowTracker = shell.WindowTracker.get_default();

  // activate terminal app;
  windowManager.removeKeybinding("switch-to-application-1");
  windowManager.addKeybinding(
    "switch-to-application-1",
    new imports.gi.Gio.Settings({ schema_id: imports.ui.windowManager.SHELL_KEYBINDINGS_SCHEMA }),
    meta.KeyBindingFlags.IGNORE_AUTOREPEAT,
    shell.ActionMode.NORMAL | shell.ActionMode.OVERVIEW,
    (_display, _window, _binding) => {
      if (overview.visible) overview.hide();

      const termApp = appSystem.lookup_app("lxterminal.desktop");

      const focusedWindow = global.display.get_focus_window();
      if (!focusedWindow) {
        termApp.activate();
        return;
      }
      const focusedApp = windowTracker.get_window_app(focusedWindow);
      if (focusedApp == null || focusedApp.is_window_backed()) {
        termApp.activate();
        return;
      }
      const focusedAppId = focusedApp.get_id();
      if (focusedAppId === "lxterminal.desktop") {
        termApp.open_new_window(-1);
      } else {
        termApp.activate();
      }
    }
  );

  // put terminal windows at the end when they are unfocused;
  global.display.connect("notify::focus-window", () => {
    const termApp = appSystem.lookup_app("lxterminal.desktop");

    const focusedWindow = global.display.get_focus_window();
    if (!focusedWindow) {
      termApp.get_windows().map(win => win.minimize());
      return;
    }
    const focusedApp = windowTracker.get_window_app(focusedWindow);
    if (focusedApp == null || focusedApp.is_window_backed()) {
      termApp.get_windows().map(win => win.minimize());
      return;
    }
    const focusedAppId = focusedApp.get_id();
    if (focusedAppId === "lxterminal.desktop") {
      termApp.get_windows().reverse().map(win => {
        if (win.minimized) win.unminimize();
        else win.focus();
      });
    } else {
      termApp.get_windows().map(win => win.minimize());
    }
  });
}
}
