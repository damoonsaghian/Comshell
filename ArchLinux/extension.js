function init() {
const main = imports.ui.main;
const GLib = imports.gi.GLib;
const GObject = imports.gi.GObject;
const St = imports.gi.St;
const Clutter = imports.gi.Clutter;
const Meta = imports.gi.Meta;
const Shell = imports.gi.Shell;
const appSystem = Shell.AppSystem.get_default();
const windowTracker = Shell.WindowTracker.get_default()

main.setThemeStylesheet("/usr/local/share/gnome-shell/extensions/gnome-shell-improved/style.css");
main.loadTheme();

// autostart
global.run_at_leisure(() => {
  const apps = ["comshell.desktop", "atom.desktop", "code-oss.desktop", "emacs.desktop",
    "nvim-qt.desktop", "nvim.desktop", "vim.desktop", "org.gnome.Terminal.desktop"];
  for (const appId of apps) {
    const app = appSystem.lookup_app(appId);
    if (app) {
      app.activate();
      break;
    }
  }
});

// no overview at start_up;
{
  let signal;
  signal = main.overview.connect('shown', (overview) => {
    overview.hide();
    overview.disconnect(signal);
  });
}
// hide dash in the overview;
main.overview.connect("showing", (overview) => overview.dash.hide());
// hide workspaces display in the overview;
main.overview.connect("showing", (overview) => overview._overview._controls._workspacesDisplay.hide());

// toggle applications view instead of overview;
main.overview.toggle = function () {
  if (this.isDummy) return;
  if (this.visible) this.hide();
  else this.showApps();
};
main.wm.removeKeybinding("toggle-application-view");
main.wm.addKeybinding(
  "toggle-application-view",
  new imports.gi.Gio.Settings({ schema_id: imports.ui.windowManager.SHELL_KEYBINDINGS_SCHEMA }),
  Meta.KeyBindingFlags.IGNORE_AUTOREPEAT,
  Shell.ActionMode.NORMAL | Shell.ActionMode.OVERVIEW,
  () => main.overview.toggle()
);

// close overview by pressing "esc" key only once;
// based on _onStageKeyPress function from:
//   https://gitlab.gnome.org/GNOME/gnome-shell/-/blob/master/js/ui/searchController.js
main.overview._overview._controls._searchController._onStageKeyPress = function (actor, event) {
  // ignore events while anything but the overview has pushed a modal (system modals, looking glass, ...);
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
  }
  return Clutter.EVENT_PROPAGATE;
}

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

main.panel.statusArea.appMenu.container.connect("show", c => c.hide());
main.panel.statusArea.appMenu.container.hide();
main.panel.statusArea.dateMenu.container.connect("show", c => c.hide());
main.panel.statusArea.dateMenu.container.hide();

// over_write "_sync" method, to hide the power icon, if there's no battery;
const power = main.panel.statusArea.aggregateMenu._power;
if (power._sync && power._proxy) {
  power._sync = function() {
    imports.ui.status.power.Indicator.prototype._sync.call(this);
    if (!this._proxy.IsPresent) this._indicator.hide();
  };
  power._sync();
}

const dateTimeLabel = new St.Label({ y_align: Clutter.ActorAlign.CENTER, style: "margin: 0 0 0 8px" });
main.panel.statusArea.aggregateMenu._indicators?.add_child(dateTimeLabel);
const updateClock = () => {
  const now = GLib.DateTime.new_now_local();
  const nowFormated = now ? now.format("%F  %a  %p  %I:%M  ") : "";
  dateTimeLabel.set_text(nowFormated);
};
updateClock();
const wallClock = main.panel.statusArea.dateMenu._clock;
if (wallClock) wallClock.connect("notify::clock", updateClock);

imports.misc.extensionUtils.getCurrentExtension().imports.system_monitor;

// -----------------
// running apps list
// -----------------
const activitiesButton = main.panel.statusArea.activities;
activitiesButton.remove_all_children();
const runningAppsBox = new St.BoxLayout({ x_align: Clutter.ActorAlign.CENTER });
activitiesButton.add_child(runningAppsBox);
runningAppsBox.add_child(new St.Icon({
  icon_name: "view-app-grid-symbolic",
  y_align: Clutter.ActorAlign.CENTER,
  icon_size: 30
}));

const CyclerHighlight = imports.ui.altTab.CyclerHighlight;
let highlights = [];

let appSwitchMode = false;
let appIndex = 0;

main.wm.setCustomKeybindingHandler(
  "switch-applications",
  Shell.ActionMode.NORMAL | Shell.ActionMode.OVERVIEW,
  (_display, _win, _binding) => {
    const overview = main.overview;
    if (overview.visible) { overview.hide(); return; }

    // could not use "appSystem.get_running()" due to a bug in gnome-shell
    //   which causes some apps icons appear 10 seconds after launch;
    let apps = global.display.get_tab_list(Meta.TabList.NORMAL, null)
      .map(win => windowTracker.get_window_app(win));
    // remove duplicates (this method preserves the order of the original array):
    apps = Array.from(new Set(apps));

    if (apps.length === 0) return;

    const nextAppIndex = (appIndex + 1) % apps.length;

    // show app's windows without focusing it;
    highlights.forEach(highlight => highlight.destroy());
    highlights = [];
    apps[nextAppIndex].get_windows().forEach(win => {
      const highlight = new CyclerHighlight();
      highlight.window = win;
      global.window_group.add_child(highlight);
      highlights.unshift(highlight);
    });
    highlights.forEach(highlight => global.window_group.set_child_above_sibling(highlight, null));

    // update indicator's highlight;
    apps[appIndex].indicator_.set_background_color(Clutter.Color.new(255, 255, 255, 0));
    apps[nextAppIndex].indicator_.set_background_color(Clutter.Color.new(255, 255, 255, 150));

    appIndex = nextAppIndex;

    if (appSwitchMode) return;
    appSwitchMode = true;

    if (!global.begin_modal(global.get_current_time(), 0)) {
      // probably someone else has a pointer grab, try again with keyboard only;
      if (!global.begin_modal(global.get_current_time(), Meta.ModalOptions.POINTER_ALREADY_GRABBED))
        return;
    }

    const endAppSwitch = () => {
      highlights.forEach(highlight => highlight.destroy());
      highlights = [];

      const app = apps[appIndex];
      app?.activate();
      // due to a bug in gnome-shell (mentioned above)
      app.get_windows()[0]?.activate(global.get_current_time());

      // remove indicator's highlight;
      app?.indicator_.set_background_color(Clutter.Color.new(255, 255, 255, 0));

      appSwitchMode = false;
      appIndex = 0;
    };

    const stage = global.get_stage();

    let releaseHandler;
    releaseHandler = stage.connect("key-release-event", (_s, _keyEvent) => {
      const [_x, _y, mods] = global.get_pointer();
      if (!mods) {
        stage.disconnect(releaseHandler);
        global.end_modal(global.get_current_time());
        endAppSwitch();
      }
      return true;
    });

    // there's a race condition;
    // if the user released Alt before we got the grab, then we won't be notified; so we check now;
    const [_x, _y, mods] = global.get_pointer();
    if (!mods) {
      stage.disconnect(releaseHandler);
      global.end_modal(global.get_current_time());
      endAppSwitch();
    }
  }
);

let windowSwitchMode = false;
let windowIndex = 0;
let highlight = null;

main.wm.setCustomKeybindingHandler(
  "switch-group",
  Shell.ActionMode.NORMAL | Shell.ActionMode.OVERVIEW,
  (_display, _win, _binding) => {
    const overview = main.overview;
    if (overview.visible) { overview.hide(); return; }

    const app = windowTracker.get_window_app(global.display.focus_window);
    if (!app) return;
    const windows = app.get_windows().filter((win) => win.get_transient_for() === null);
    if (windows.length === 0) return;

    const nextWindowIndex = (windowIndex + 1) % windows.length;

    // show window without focusing it;
    if (!highlight) {
      highlight = new CyclerHighlight();
      global.window_group.add_child(highlight);
    }
    highlight.window = windows[nextWindowIndex];
    global.window_group.set_child_above_sibling(highlight, null);

    // update the windows indicator of the app;
    app.indicator_.updateWindowsIndicator(nextWindowIndex);

    windowIndex = nextWindowIndex;

    if (windowSwitchMode) return;
    windowSwitchMode = true;

    if (!global.begin_modal(global.get_current_time(), 0)) {
      // probably someone else has a pointer grab, try again with keyboard only;
      if (!global.begin_modal(global.get_current_time(), Meta.ModalOptions.POINTER_ALREADY_GRABBED))
        return;
    }

    const stage = global.get_stage();

    const endWindowSwitch = () => {
      highlight.destroy();
      highlight = null;

      const app = windowTracker.get_window_app(global.display.focus_window);
      app.get_windows()[windowIndex]?.activate(global.get_current_time());

      // reset the windows indicator of the app;
      app?.indicator_.updateWindowsIndicator();

      windowSwitchMode = false;
      windowIndex = 0;
    };

    let releaseHandler;
    releaseHandler = stage.connect("key-release-event", (_s, _keyEvent) => {
      const [_x, _y, mods] = global.get_pointer();
      if (!mods) {
        stage.disconnect(releaseHandler);
        global.end_modal(global.get_current_time());
        endWindowSwitch();
      }
      return true;
    });

    // there's a race condition;
    // if the user released Alt before we got the grab, then we won't be notified; so we check now;
    const [_x, _y, mods] = global.get_pointer();
    if (!mods) {
      stage.disconnect(releaseHandler);
      global.end_modal(global.get_current_time());
      endWindowSwitch();
    }
  }
);

const AppIndicator = GObject.registerClass(
class AppIndicator extends St.BoxLayout {
  _init(app) {
    super._init({ x_expand: true, style: "padding: 1px 4px" });
    app.indicator_ = this;
    this.app = app;

    const icon = new St.Icon({ icon_size: 30 });
    icon.set_gicon(app.get_icon());
    this.add_child(icon);

    this.windowsIndicator = new St.Label({
      y_align: Clutter.ActorAlign.CENTER,
      style: "color: #00CCFF; font-family: monospace"
    });
    this.add_child(this.windowsIndicator);
    this.signal = app.connect("windows-changed", () => this.updateWindowsIndicator());
    this.updateWindowsIndicator();
  }

  updateWindowsIndicator(index = 0) {
    const nWindows = this.app.get_windows().filter((win) => win.get_transient_for() === null).length;
    let indicator = "";
    if (nWindows > 0 && index <= 0) {
      indicator = "◻".repeat(nWindows - 1);
    } else if (index > 0 && nWindows - index > 0) {
      indicator = "◻".repeat(index - 1) + "◼" + "◻".repeat(nWindows - index - 1);
    }
    this.windowsIndicator.set_text(indicator);

    if (this.app.get_n_windows() > 0) return;
    this.app.disconnect(this.signal);
    delete this.app.indicator_;
  }
});

windowTracker.connect("notify::focus-app", () => {
  // this is due to a bug: apps with a transient window are not moved to the head of "tab_list", when activated;
  windowTracker.focus_app?.get_windows()
    .filter((win) => win.get_transient_for() === null)[0]
    .activate(global.get_current_time());

  let apps = global.display.get_tab_list(Meta.TabList.NORMAL, null)
    .map(win => windowTracker.get_window_app(win));
  // remove duplicates (this method preserves the order of the original array):
  apps = Array.from(new Set(apps));

  runningAppsBox.remove_all_children();

  if (apps.length === 0) {
    const appsGridIcon = new St.Icon({
      icon_name: "view-app-grid-symbolic",
      y_align: Clutter.ActorAlign.CENTER,
      icon_size: 30
    });
    runningAppsBox.add_child(appsGridIcon);
    main.overview.showApps();
    return;
  }

  apps.forEach(app => {
    const indicator = app.indicator_;
    if (indicator) {
      runningAppsBox.add_child(indicator);
    } else {
      runningAppsBox.add_child(new AppIndicator(app));
    }
  });

  // to deal with windows launched during switching:
  apps[appIndex]?.indicator_.set_background_color(Clutter.Color.new(255, 255, 255, 0));
  appSwitchMode = false;
  appIndex = 0;
  apps[0]?.indicator_.updateWindowsIndicator();
  windowSwitchMode = false;
  windowIndex = 0;
});

return { enable: () => {}, disable: () => {} };
}
