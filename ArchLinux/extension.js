function init() {
const main = imports.ui.main;
const GLib = imports.gi.GLib;
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

  // show a red indicator to notify the user that for system to update, it needs a reboot;
  // compare the running kernel and system services, with the ones on the disk;
  // https://github.com/RaphaelRochet/arch-update

  const network = main.panel.statusArea.aggregateMenu._network;
  if (network && network._primaryIndicator) {
    network.remove_child(network._primaryIndicator);
    rightBox.add_child(network._primaryIndicator);
    if (network._vpnIndicator) {
      network.remove_child(network._vpnIndicator);
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

  const power = main.panel.statusArea.aggregateMenu._power;
  if (power && power._indicator) {
    power.remove_child(power._indicator);
    rightBox.add_child(power._indicator);
    if (power._percentageLabel) {
      power.remove_child(power._percentageLabel);
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

  const volume = main.panel.statusArea.aggregateMenu._volume;
  if (volume && volume._primaryIndicator) {
    volume.remove_child(volume._primaryIndicator);
    rightBox.add_child(volume._primaryIndicator);
    if (volume._inputIndicator) {
      volume.remove_child(volume._inputIndicator);
      rightBox.add_child(volume._inputIndicator);
      volume._inputIndicator.visible = true;
    }
  }

  const remoteAccess = main.panel.statusArea.aggregateMenu._remoteAccess;
  if (remoteAccess) {
    remoteAccess._ensureControls();
    if (remoteAccess._recordingIndicator) {
      remoteAccess.remove_child(remoteAccess._recordingIndicator);
      rightBox.add_child(remoteAccess._recordingIndicator);
      remoteAccess._sync();
    }
  }

  const location = main.panel.statusArea.aggregateMenu._location;
  if (location && location._indicator) {
    location.remove_child(location._indicator);
    rightBox.add_child(location._indicator);
    location._syncIndicator();
  }

  const dateTimeLabel = new St.Label({ y_align: Clutter.ActorAlign.CENTER });
  rightBox.add_child(dateTimeLabel);
  const updateClock = () => {
    const now = GLib.DateTime.new_now_local();
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
    layout_manager: new Clutter.BoxLayout({ spacing: 18 }),
    x_align: Clutter.ActorAlign.START,
    x_expand: true,
    y_expand: true,
  });
  leftBox.add_child(workspacesIndicator);

  const workspaceManager = global.workspace_manager;
  const workspaceList = [];

  const nextWorkspace = () => {
    const activeWorkspaceIndex = workspaceList.indexOf(workspaceManager.get_active_workspace());

    const nextWorkspaceIndex = (activeWorkspaceIndex + 1) % workspaceList.length;
    const nextWorkspace = workspaceList[nextWorkspaceIndex];
    if (!nextWorkspace) return;
    nextWorkspace.activate(global.get_current_time());
  };

  const endWorkspaceSwitch = () => {
    if (workspaceList[0]?.name_.startsWith("*")) {
      const indicator = firstWorkspace.indicator_;
      workspacesIndicator.remove_child(indicator);
      workspacesIndicator.add_child(indicator);
    }

    let activeWorkspaceIndex = workspaceList.indexOf(workspaceManager.get_active_workspace());
    if (activeWorkspaceIndex <= 0) return;

    const activeWorkspace = workspaceList.splice(activeWorkspaceIndex, 1)[0];
    workspaceList.unshift(activeWorkspace);

    const indicator = activeWorkspace.indicator_;
    workspacesIndicator.set_child_at_index(indicator, 0);
    indicator?.set_background_color(Clutter.Color.new(255, 255, 255, 0));
  };

  let workspaceSwitchMode = false;

  main.wm.setCustomKeybindingHandler(
    "switch-applications",
    Shell.ActionMode.NORMAL | Shell.ActionMode.OVERVIEW,
    (_display, _win, _binding) => {
      const overview = main.overview;
      if (overview.visible) { overview.hide(); return; }

      nextWorkspace();
      if (workspaceSwitchMode) return;
      workspaceSwitchMode = true;

      if (!global.begin_modal(global.get_current_time(), 0)) {
        // probably someone else has a pointer grab, try again with keyboard only;
        if (!global.begin_modal(global.get_current_time(), Meta.ModalOptions.POINTER_ALREADY_GRABBED))
          return;
      }

      const stage = global.get_stage();

      let releaseHandler;
      releaseHandler = stage.connect("key-release-event", (_s, _keyEvent) => {
        const [_x, _y, mods] = global.get_pointer();
        if (!mods) {
          stage.disconnect(releaseHandler);
          global.end_modal(global.get_current_time());
          workspaceSwitchMode = false;
          endWorkspaceSwitch();
        }
        return true;
      });

      // there's a race condition;
      // if the user released Alt before we got the grab, then we won't be notified; so we check now;
      const [_x, _y, mods] = global.get_pointer();
      if (!mods) {
        stage.disconnect(releaseHandler);
        global.end_modal(global.get_current_time());
        workspaceSwitchMode = false;
        endWorkspaceSwitch();
      }
    }
  );

  main.wm.setCustomKeybindingHandler(
    "cycle-windows",
    Shell.ActionMode.NORMAL,
    (_display, _win, _binding) => {
      const activeWorkspace = workspaceManager.get_active_workspace();
      const windows = activeWorkspace.windows_;
      const win = global.display.get_focus_window();
      const i = windows.indexOf(win);
      windows[(i + 1) % windows.length].activate(global.get_current_time());
    }
  );

  const WindowsIndicator = imports.gi.GObject.registerClass(
  class WindowsIndicator extends St.Label {
    _init(workspace) {
      super._init();
      workspace.windows_ = [];

      workspace.connect("window-added", (workspace, win) => {
        if (win.get_window_type() === Meta.WindowType.NORMAL) {
          workspace.windows_.push(win);
          this.onWindowsChanged(workspace);
        }
      });

      workspace.connect("window-removed", (workspace, win) => {
        const i = workspace.windows_.indexOf(win);
        if (i === -1) return;
        workspace.windows_.splice(i, 1); // remove "win" from the list;
        this.onWindowsChanged(workspace);
      });

      global.display.connect("notify::focus-window", () => this.onWindowsChanged(workspace));
      this.onWindowsChanged(workspace);
    }

    onWindowsChanged(workspace) {
      let indicator = "";
      const windows = workspace.windows_;
      for (let i = 1; i < windows.length; i++) {
        if (windows[i].appears_focused) {
          indicator += "┃";
        } else {
          indicator += "┇";
        }
      }
      this.set_text(indicator);
    }
  });

  // a map from names to workspaces;
  const workspaces = new Map();

  const openApp = (app) => {
    const appName = app.get_name();

    let workspace = workspaces.get(appName);
    if (workspace) {
      workspace.activate(global.get_current_time());
      endWorkspaceSwitch();
      return;
    }

    workspace = workspaceManager.append_new_workspace(true, global.get_current_time());
    main.wm._workspaceTracker.blockUpdates();
    app.open_new_window(workspace.index());

    const indicator = new St.BoxLayout({ x_expand: true });
    workspace.indicator_ = indicator;
    const icon = new St.Icon();
    icon.set_icon_size(16);
    icon.set_gicon(app.get_icon());
    indicator.add(icon);
    const windowsIndicator = new WindowsIndicator(workspace);
    windowsIndicator.set_style("color: #00CCFF"); // #99FFFF #87CEEB
    indicator.add(windowsIndicator);
    const label = St.Label.new(appName.replace(/ .*/,'')); // first word of "appName"
    indicator.add(label);

    workspace.connect("notify::active", () => {
      if (workspace.active && workspaceList.indexOf(workspace) !== 0) {
        // highlight
        workspace.indicator_?.set_background_color(Clutter.Color.new(255, 255, 255, 150));
      } else {
        // clear highlight
        workspace.indicator_?.set_background_color(Clutter.Color.new(255, 255, 255, 0));
      }
    });

    workspace.connect("window-added", (workspace, win) => {
      if (workspace.n_windows === 1) {
        main.wm._workspaceTracker.unblockUpdates();
        if (workspaces.get(appName)) {
          win.kill();
          return;
        }

        workspace.name_ = appName;
        workspace.activate(global.get_current_time());
        workspacesIndicator.insert_child_at_index(workspace.indicator_, 0);
        workspaces.set(appName, workspace);
        workspaceList.unshift(workspace);
        endWorkspaceSwitch();

        win.maximize(Meta.MaximizeFlags.BOTH);
      }
    });

    workspace.connect("window-removed", (workspace, _win) => {
      if (workspace.n_windows === 0) {
        if (!workspace.name_) return;
        workspacesIndicator.remove_child(workspace.indicator_);
        workspaces.delete(workspace.name_);
        const i = workspaceList.indexOf(workspace);
        workspaceList.splice(i, 1);

        const firstWorkspace = workspaceList[0];
        if (firstWorkspace) firstWorkspace.activate(global.get_current_time());
      }
    });
  };

  Shell.App.prototype.activate = function() { openApp(this); };

  // autostart
  global.run_at_leisure(() => {
    const apps = ["comshell.desktop", "atom.desktop", "code-oss.desktop", "emacs.desktop",
      "nvim-qt.desktop", "nvim.desktop", "vim.desktop", "org.gnome.Terminal.desktop"];
    for (const appId of apps) {
      const app = Shell.AppSystem.get_default().lookup_app(appId);
      if (app) {
        openApp(app);
        break;
      }
    }
  });
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
