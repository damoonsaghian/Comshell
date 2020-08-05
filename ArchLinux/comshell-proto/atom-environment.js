const WindowEventHandler = require('./window-event-handler');
const StateStore = require('./state-store');
const registerDefaultCommands = require('./register-default-commands');
const ConfigSchema = require('./config-schema');
const Config = require('./config');
const DeserializerManager = require('./deserializer-manager');
const ViewRegistry = require('./view-registry');
const NotificationManager = require('./notification-manager');
const KeymapManager = require('./keymap-extensions');
const TooltipManager = require('./tooltip-manager');
const CommandRegistry = require('./command-registry');
const StyleManager = require('./style-manager');
const Project = require('./project');
const Workspace = require('./workspace');
const PaneContainer = require('./pane-container');
const PaneAxis = require('./pane-axis');
const Pane = require('./pane');
const Dock = require('./dock');
const GrammarRegistry = require('./editor/grammar-registry');
const TextEditorRegistry = require('./editor/text-editor-registry');
const TextEditor = require('./editor/text-editor');
const Clipboard = require('./clipboard');

const TextBuffer = require('text-buffer');

class ApplicationDelegate {
  constructor() {}

  getWindowLoadSettings() {
    const userDataDir = nw.App.fullArgv['--user-data-dir'];
    const projectRootPath = userDataDir ? require('path').join(userDataDir, '../..') : null;
    const initialProjectRoots = projectRootPath ? [projectRootPath] : null;
    return {
      appName: 'Comshell',
      resourcePath: __dirname,
      atomHome: process.env.ATOM_HOME,
      initialProjectRoots
    };
  }

  getCurrentWindow() {
    return nw.Window.get();
  }

  closeWindow() {
    nw.Window.get().close();
    return Promise(resolve => resolve());
  }

  async getTemporaryWindowState() {
    return nw.Window.get().temporaryState;
  }

  setTemporaryWindowState(state) {
    nw.Window.get().temporaryState = state;
    return Promise(resolve => resolve());
  }

  getWindowSize() {
    const win = nw.Window.get();
    const width = win.width;
    const height = win.height;
    return { width, height };
  }

  setWindowSize(width, height) {
    const win = nw.Window.get();
    win.width = width;
    win.height = height;
    return Promise(resolve => resolve());
  }

  getWindowPosition() {
    const win = nw.Window.get();
    const x = win.x;
    const y = win.y;
    return { x, y };
  }

  setWindowPosition(x, y) {
    const win = nw.Window.get();
    win.x = x;
    win.y = y;
    return Promise(resolve => resolve());
  }

  centerWindow() {
    nw.Window.get().setPosition('center');
    return Promise(resolve => resolve());
  }

  focusWindow() {
    nw.Window.get().focus();
    return Promise(resolve => resolve());
  }

  showWindow() {
    nw.Window.get().show();
    return Promise(resolve => resolve());
  }

  hideWindow() {
    nw.Window.get().hide();
    return Promise(resolve => resolve());
  }

  reloadWindow() {
    nw.Window.get().reload();
    return Promise(resolve => resolve());
  }

  restartApplication() {
    nw.App.quit();
    return Promise(resolve => resolve());
  }

  minimizeWindow() {
    nw.Window.get().minimize();
    return Promise(resolve => resolve());
  }

  isWindowMaximized() {
    nw.Window.get().maximize();
    return true;
  }

  maximizeWindow() {
    nw.Window.get().maximize();
    return Promise(resolve => resolve());
  }

  unmaximizeWindow() {
    nw.Window.get().unmaximize();
    return Promise(resolve => resolve());
  }

  isWindowFullScreen() {
    return nw.Window.get().isFullscreen();
  }

  setWindowFullScreen(fullScreen = false) {
    nw.Window.get().enterFullscreen();
    return Promise(resolve => resolve());
  }

  openExternal(url) {
    nw.Shell.openExternal(url);
    return Promise(resolve => resolve());
  }
}

require('util').promisify(fs.stat); // stat 1608
require('crypto') // crypto 1501
// if initialProjectRoots is empty show projects list;

// Essential: Atom global.
//
// An instance of this class is always available as the `atom` global.
class AtomEnvironment {
  clipboard = new Clipboard();
  deserializers;
  views;
  notifications = new NotificationManager();
  config = new Config();
  keymaps;
  commands = new CommandRegistry();
  grammars;
  styles = new StyleManager();
  project;
  textEditors;
  workspace;
  history;

  constructor() {
    this.applicationDelegate = new ApplicationDelegate();

    TextEditor.setClipboard(this.clipboard);
    TextEditor.setScheduler(this.views);
  }
}
