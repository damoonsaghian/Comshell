function init() {
const main = imports.ui.main;
const GObject = imports.gi.GObject;
const St = imports.gi.St;
const Clutter = imports.gi.Clutter;
const Meta = imports.gi.Meta;
const Shell = imports.gi.Shell;

main.setThemeStylesheet(
  "/usr/local/share/gnome-shell/extensions/gnome-shell-improved/style.css");
main.loadTheme();

// hide notification banners;
{
  const bannerBin = main.messageTray._bannerBin;
  if (bannerBin) bannerBin.hide();
}

// move status_bar to the bottom;
{
  const panelBox = main.layoutManager.panelBox;
  function movePanelToBottom() {
    const monitor = main.layoutManager.primaryMonitor;
    const y = monitor.height - panelBox.height;
    panelBox.set_position(0, y);
  }
  main.layoutManager.connect("monitors-changed", movePanelToBottom);
  panelBox.connect("notify::height", movePanelToBottom);
  movePanelToBottom();
}

main.panel.statusArea.activities.container.connect("show", c => c.hide());
main.panel.statusArea.activities.container.hide();
main.panel.statusArea.appMenu.container.connect("show", c => c.hide());
main.panel.statusArea.appMenu.container.hide();
main.panel.statusArea.dateMenu.container.connect("show", c => c.hide());
main.panel.statusArea.dateMenu.container.hide();
main.panel.statusArea.aggregateMenu.container.connect("show", c => c.hide());
main.panel.statusArea.aggregateMenu.container.hide();

// right side of status_bar;
{
  const rightButton = new imports.ui.panelMenu.Button(0.0, null, true);
  main.panel.addToStatusArea("status_right", rightButton, 0, "right");
  const rightBox = new St.BoxLayout({ style_class: "panel-status-indicators-box" });
  rightButton.add_child(rightBox);

  const screencast = main.panel.statusArea.aggregateMenu._screencast;
  if (screencast && screencast._indicator) {
    screencast.indicators.remove_child(screencast._indicator);
    rightBox.add_child(screencast._indicator);
    screencast._sync();
  }
  /*
  // since screencast implementation is simple, an alternative method is:
  const screencastIcon = new St.Icon({ style_class: "system-status-icon" });
  rightBox.add_child(screencastIcon);
  screencastIcon.icon_name = "media-record-symbolic";
  screencastIcon.add_style_class_name("screencast-indicator");
  main.screencastService.connect("updated",
    () => screencastIcon.visible = main.screencastService.isRecording
  );
  */

  // show a red indicator to notify the user that for system to update, it needs a reboot;
  // compare the running kernel and system services, with the ones on the disk;
  // https://github.com/RaphaelRochet/arch-update

  const remoteAccess = main.panel.statusArea.aggregateMenu._remoteAccess;
  if (remoteAccess && remoteAccess._indicator) {
    remoteAccess._ensureControls();
    remoteAccess.indicators.remove_child(remoteAccess._indicator);
    rightBox.add_child(remoteAccess._indicator);
    remoteAccess._sync();
  }

  const network = main.panel.statusArea.aggregateMenu._network;
  if (network && network._primaryIndicator) {
    network.indicators.remove_child(network._primaryIndicator);
    rightBox.add_child(network._primaryIndicator);
    if (network._vpnIndicator) {
      network.indicators.remove_child(network._vpnIndicator);
      rightBox.add_child(network._vpnIndicator);
    }
  }

  // https://github.com/hedayaty/NetSpeed/blob/master/net_speed.js
  // https://github.com/hedayaty/NetSpeed/blob/master/net_speed_status_icon.js

  const rfkillIcon = new St.Icon({ style_class: "system-status-icon" });
  rightBox.add_child(rfkillIcon);
  rfkillIcon.icon_name = "airplane-mode-symbolic";
  const rfkillManager = imports.ui.status.rfkill.getRfkillManager();
  rfkillIcon.visible = rfkillManager.airplaneMode;
  rfkillManager.connect("airplane-mode-changed",
    () => rfkillIcon.visible = rfkillManager.airplaneMode
  );

  const volume = main.panel.statusArea.aggregateMenu._volume;
  if (volume && volume._primaryIndicator) {
    volume.indicators.remove_child(volume._primaryIndicator);
    rightBox.add_child(volume._primaryIndicator);
    if (volume._inputIndicator) {
      volume.indicators.remove_child(volume._inputIndicator);
      rightBox.add_child(volume._inputIndicator);
      volume._inputIndicator.visible = true;
    }
  }

  const power = main.panel.statusArea.aggregateMenu._power;
  if (power && power._indicator) {
    power.indicators.remove_child(power._indicator);
    rightBox.add_child(power._indicator);
    if (power._percentageLabel) {
      power.indicators.remove_child(power._percentageLabel);
      rightBox.add_child(power._percentageLabel);
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

  const location = main.panel.statusArea.aggregateMenu._location;
  if (location && location._indicator) {
    location.indicators.remove_child(location._indicator);
    rightBox.add_child(location._indicator);
    location._syncIndicator();
  }

  const dateTimeLabel = new St.Label({ y_align: Clutter.ActorAlign.CENTER });
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
  const leftBox = new St.BoxLayout({ style_class: "panel-status-indicators-box" });
  leftButton.add_child(leftBox);

  const workspacesIndicator = new St.Widget({
    layout_manager: new Clutter.BoxLayout({ homogeneous: true , spacing: 8}),
    x_align: Clutter.ActorAlign.START,
    x_expand: true,
    y_expand: true,
  });
  leftBox.add_child(workspacesIndicator);

  const WindowsIndicator = GObject.registerClass(
  class WindowsIndicator extends St.Label {
    windows = [];

    _init(workspace) {
      super._init("");

      this.onWindowsChanged();
      workspace.connect("window-added", (_w, win) => {
        this.windows.push(win);
        this.onWindowsChanged();
      });
      workspace.connect("window-removed", (_w, win) => {
        const i = this.windows.indexOf(win);
        // remove "win" from "this.windows" list;
        this.windows.splice(i, 1);
        this.onWindowsChanged();
        // if the closed window is the main window, close other windows of the workspace too;
        if (i===0) this.windows.forEach(win => win.delete(global.get_current_time()));
      });
    }

    onWindowsChanged() {
      let indicator = "";
      this.windows.filter(win => !win.is_attached_dialog()).forEach(win => {
        if (win.appears_focused()) {
          indicator += '<span foreground="blue">┃</span>';
        } else {
          indicator += "┃";
        }
      });
      this.clutter_text.set_markup(indicator);
    }
  });

  global.display.connect('notify::focus-window', () => {
    const windowsIndicator = global.display.get_focus_window().getWorkspace().windowsIndicator;
    if (windowsIndicator) windowsIndicator.onWindowsChanged();
  });

  main.wm.setCustomKeybindingHandler(
    "cycle-windows",
    Shell.ActionMode.NORMAL,
    (display, win, binding) => {
      const focused_window = global.display.get_focus_window();
      const current_workspace_windows = focused_window.getWorkspace().windowsIndicator.windows;
      const i = current_workspace_windows.indexOf(focused_window);
      current_workspace_windows[i+1].focus(global.get_current_time());
    }
  );

  const workspaces = new Map();

  const openApp = (app) => {
    const workspace = workspaces[app.get_name()];
    const workspace_manager = global.workspace_manager;

    if (workspace) {
      workspace.activate(global.get_current_time())
      workspacesIndicator.set_child_at_index(workspace.windowsIndicator.get_parent(), 0);
      return;
    }

    app.open_new_window(-1);
    const indicator = new St.BoxLayout({ x_expand: true });
    workspacesIndicator.add_child(indicator);
    workspacesIndicator.set_child_at_index(indicator, 0);
    const icon = new St.Icon();
    icon.set_gicon(app.get_icon());
    indicator.add(icon);
    const windowsIndicator = new WindowsIndicator(workspace);
    indicator.add(windowsIndicator);
    workspace.windowsIndicator = windowsIndicator;
  };

  //imports.ui.appDisplay.AppIcon.prototype.activate = function() { openApp(this.app) };

  // when workspace is empty, go to the previous workspace;
  // https://github.com/rliang/gnome-shell-extension-overview-when-empty
  // https://extensions.gnome.org/extension/2036/show-applicativon-view-when-workspace-empty/

  global.workspace_manager.connect("workspace-switched", (fromWorkspace, toWorkspace, _d) => {
    // clear the background color of indicator of the previous workspace;
    fromWorkspace.windowsIndicator.get_parent().set_background_color(new Clutter.Color(255, 255, 255, 50));
    // highlight the background of the indicator of the selected workspace;
    toWorkspace.windowsIndicator.get_parent().set_background_color(new Clutter.Color(255, 255, 255, 0));
  });

  main.wm.setCustomKeybindingHandler(
    "switch-applications",
    Shell.ActionMode.NORMAL | Shell.ActionMode.OVERVIEW,
    (display, win, binding) => {
      const overview = main.overview;
      if (overview.visible) { overview.hide(); return; }
      // this line is temporary:
      main.wm._startSwitcher(display, win, binding);
      // start workspace switcher
      //for (let i = workspaces.size - 1; i >=0; i--) {}
      //workspacesIndicator.set_child_at_index(indicator, 0);
      // clear the background color of the indicator;
      //indicator.set_background_color(new Clutter.Color(255, 255, 255, 0));
    }
  );
}

// overview layer
{
  const overview = main.overview;
  const viewSelector = overview.viewSelector;

  const toggleOverview = () => {
    if (overview.isDummy) return;
    if (overview.visible) overview.hide();
    else viewSelector.showApps();
  }

  // toggle overview when toggling apps view;
  main.wm.removeKeybinding("toggle-application-view");
  main.wm.addKeybinding(
    "toggle-application-view",
    new imports.gi.Gio.Settings({ schema_id: imports.ui.windowManager.SHELL_KEYBINDINGS_SCHEMA }),
    Meta.KeyBindingFlags.IGNORE_AUTOREPEAT,
    Shell.ActionMode.NORMAL | Shell.ActionMode.OVERVIEW,
    toggleOverview
  );

  // toggle overview when clicking on an empty area on the panel;
  main.panel.connect("button-press-event", toggleOverview);

  // close overview by pressing "esc" key only once;
  // based on _onStageKeyPress function from:
  //   https://gitlab.gnome.org/GNOME/gnome-shell/-/blob/master/js/ui/viewSelector.js
  viewSelector._onStageKeyPress = function (actor, event) {
    if (main.modalCount > 1)
      return Clutter.EVENT_PROPAGATE;

    let symbol = event.get_key_symbol();

    if (symbol === Clutter.KEY_Escape) {
      if (this._searchActive)
        this.reset();
      else
        main.overview.hide();
      return Clutter.EVENT_STOP;
    } else if (this._shouldTriggerSearch(symbol)) {
        this.startSearch(event);
    } else if (!this._searchActive && !global.stage.key_focus) {
      if (symbol === Clutter.KEY_Tab || symbol === Clutter.KEY_Down) {
        this._activePage.navigate_focus(null, St.DirectionType.TAB_FORWARD, false);
        return Clutter.EVENT_STOP;
      } else if (symbol === Clutter.KEY_ISO_Left_Tab) {
        this._activePage.navigate_focus(null, St.DirectionType.TAB_BACKWARD, false);
        return Clutter.EVENT_STOP;
      }
    }
    return Clutter.EVENT_PROPAGATE;
  }

  // hide dash;
  overview.connect("showing", () => overview.dash.hide());
}

return { enable: () => {}, disable: () => {} };
}
