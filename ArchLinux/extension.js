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
/*
  const windowList = new St.Widget({
    layout_manager: new Clutter.BoxLayout({ homogeneous: true , spacing: 8}),
    x_align: Clutter.ActorAlign.START,
    x_expand: true,
    y_expand: true,
  });
  leftBox.add_child(windowList);

  const AppButton = GObject.registerClass(
  class AppButton extends St.Button {
    _init(app) {
      super._init({
        can_focus: true,
        x_expand: true,
        button_mask: St.ButtonMask.ONE | St.ButtonMask.THREE,
      });

      this.connect('notify::allocation', this._updateIconGeometry.bind(this));
      this.connect('destroy', this._onDestroy.bind(this));

      this.app = app;

      this._multiWindowTitle = new St.BoxLayout({
        x_expand: true,
      });
      this.set_child(this._multiWindowTitle);

      this._icon = new St.Bin({
        style_class: 'window-button-icon',
        child: app.create_icon_texture(ICON_TEXTURE_SIZE),
      });
      this._multiWindowTitle.add(this._icon);

      let label = new St.Label({
        text: app.get_name(),
        y_align: Clutter.ActorAlign.CENTER,
      });
      this._multiWindowTitle.add(label);
      this._multiWindowTitle.label_actor = label;

      this._textureCache = St.TextureCache.get_default();
      this._iconThemeChangedId =
        this._textureCache.connect('icon-theme-changed', () => {
          this._icon.child = app.create_icon_texture(24);
        });

      this._windowsChangedId = this.app.connect(
        'windows-changed', this._windowsChanged.bind(this));
      this._windowsChanged();

      this._windowTracker = Shell.WindowTracker.get_default();
      this._notifyFocusId = this._windowTracker.connect(
        'notify::focus-app', this._updateStyle.bind(this));
      this._updateStyle();
    }

    activate() {
      let windows = this.getWindowList();
      windows[0].activate(global.get_current_time());
      if (!windows.length === 1) {
        for (let i = 0; i < windows.length; i++) {
          let windowTitle = new WindowTitle(windows[i]);
        }
      }
    }

    _updateStyle() {
      if (this._isFocused())
        this.add_style_class_name('focused');
      else
        this.remove_style_class_name('focused');
    }

    _isWindowVisible(window) {
      let workspace = global.workspace_manager.get_active_workspace();

      return !window.skip_taskbar;
    }

    _getIconGeometry() {
      let rect = new Meta.Rectangle();

      [rect.x, rect.y] = this.get_transformed_position();
      [rect.width, rect.height] = this.get_transformed_size();

      return rect;
    }

    //

    _isFocused() {
      return this._windowTracker.focus_app === this.app;
    }

    _updateIconGeometry() {
      let rect = this._getIconGeometry();

      let windows = this.app.get_windows();
      windows.forEach(w => w.set_icon_geometry(rect));
    }

    getWindowList() {
      return this.app.get_windows().filter(win => this._isWindowVisible(win));
    }

    _windowsChanged() {
      if (this._windowTitle) {
        this.metaWindow = null;
        this._windowTitle = null;
      }
      this.label_actor = this._multiWindowTitle.label_actor;
    }

    _onMenuActivate(menu, child) {
      child._window.activate(global.get_current_time());
    }

    _onDestroy() {
      this._textureCache.disconnect(this._iconThemeChangedId);
      this._windowTracker.disconnect(this._notifyFocusId);
      this.app.disconnect(this._windowsChangedId);
    }
  });

  const onWindowAdded = (_ws, win) => {
    if (win.get_monitor() !== 0) return;

    const app = Shell.WindowTracker.get_default().get_window_app(win);
    const children = windowList.get_children();
    const child = children.find(c => c.app === app);

    // activate app

    if (child) {
      // move app button to the first place;
      windowList.set_child_at_index(child, 0);
      // add an indicator to the app button;
    } else {
      const button = new AppButton(app);
      windowList.insert_child_at_index(button, 0);
      // move app button to the first place;
    }
  };

  const onWindowRemoved = (_ws, win) => {
    if (win.get_monitor() !== 0) return;

    const app = Shell.WindowTracker.get_default().get_window_app(win);
    const children = windowList.get_children();
    const child = children.find(c => c.app === app);

    // if child exists and without indicators
    //child.destroy();
    // else remove one indicator from the app button;
  };

  const workspace_manager = global.workspace_manager;
  const connect_workspace = (i) => {
    const workspace = workspace_manager.get_workspace_by_index(i);
    workspace.connect("window-added", onWindowAdded);
    workspace.connect("window-removed", onWindowRemoved);
    // when focused window changes
  };
  for (let i = 0; i < workspace_manager.n_workspaces; i++) {
    connect_workspace(i);
  }
  workspace_manager.connect("workspace-added", (_wsm, i) => connect_workspace(i));

  const windows = global.get_window_actors().sort((w1, w2) => {
    return w1.metaWindow.get_stable_sequence() - w2.metaWindow.get_stable_sequence();
  });
  for (let i = 0; i < windows.length; i++)
    onWindowAdded(null, windows[i].metaWindow);
*/
}

// overview layer
{
  const overview = main.overview;
  const viewSelector = overview.viewSelector;
  const windowManager = main.wm;

  const toggleOverview = () => {
    if (overview.isDummy) return;
    if (overview.visible) overview.hide();
    else viewSelector.showApps();
  }

  // toggle overview when toggling apps view;
  windowManager.removeKeybinding("toggle-application-view");
  windowManager.addKeybinding(
    "toggle-application-view",
    new imports.gi.Gio.Settings({ schema_id: imports.ui.windowManager.SHELL_KEYBINDINGS_SCHEMA }),
    Meta.KeyBindingFlags.IGNORE_AUTOREPEAT,
    Shell.ActionMode.NORMAL | Shell.ActionMode.OVERVIEW,
    toggleOverview
  );

  // toggle overview when clicking on an empty area on the panel;
  main.panel.connect("button-press-event", toggleOverview);

  // hide overview when pressing "alt-tab";
  windowManager.setCustomKeybindingHandler(
    "switch-applications",
    Shell.ActionMode.NORMAL | Shell.ActionMode.OVERVIEW,
    (display, win, binding) => {
      if (overview.visible) { overview.hide(); return; }
      windowManager._startSwitcher(display, win, binding);
    }
  );

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

  // https://github.com/rliang/gnome-shell-extension-overview-when-empty
  // https://extensions.gnome.org/extension/2036/show-application-view-when-workspace-empty/
}

return { enable: () => {}, disable: () => {} };
}
