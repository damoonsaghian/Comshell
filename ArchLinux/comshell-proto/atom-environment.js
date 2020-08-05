const _ = require('underscore-plus');
const { CompositeDisposable, Disposable, Emitter } = require('event-kit');
const fs = require('fs-plus');
const path = require('path');
const TextBuffer = require('text-buffer');

const WindowEventHandler = require('./window-event-handler');
const StateStore = require('./state-store');
const registerDefaultCommands = require('./register-default-commands');
const ConfigSchema = require('./config-schema');
const Config = require('./config');
const DeserializerManager = require('./deserializer-manager');
const ViewRegistry = require('./view-registry');
const NotificationManager = require('./notification-manager');
const KeymapManager = require('atom-keymap');
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

KeymapManager.prototype.loadBundledKeymaps = function () {
  keymapsPath = path.join(this.resourcePath, 'keymaps.json');
  this.loadKeymap(keymapsPath);
  this.emitter.emit('did-load-bundled-keymaps');
};

class ApplicationDelegate {
  constructor() {}

  getWindowLoadSettings() {
    const args = nw.App.fullArgv;
    const userDataDir = args[args.indexOf('--user-data-dir') + 1]; // nw.App.dataPath
    const projectRootPath = userDataDir ? path.join(userDataDir, '../..') : null;
    const initialProjectRoots = projectRootPath ? [projectRootPath] : null;
    return {
      appName: 'Comshell',
      atomHome: process.env.ATOM_HOME,
      resourcePath: __dirname,
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

// Essential: Atom global.
//
// An instance of this class is always available as the `atom` global.
class AtomEnvironment {
  static saveStateDebounceInterval = 1000;
  static enablePersistence = true;

  clipboard = new Clipboard();
  views;
  notifications = new NotificationManager();
  deserializers;
  config = new Config();
  keymaps;
  commands = new CommandRegistry();
  grammars;
  styles = new StyleManager();
  project;
  textEditors;
  workspace;

  constructor() {
    this.applicationDelegate = new ApplicationDelegate();

    this.unloading = false;
    this.emitter = new Emitter();
    this.disposables = new CompositeDisposable();

    this.views = new ViewRegistry(this);

    this.deserializers = new DeserializerManager(this);
    // register default deserializers
    this.deserializers.add(Workspace);
    this.deserializers.add(PaneContainer);
    this.deserializers.add(PaneAxis);
    this.deserializers.add(Pane);
    this.deserializers.add(Dock);
    this.deserializers.add(Project);
    this.deserializers.add(TextEditor);
    this.deserializers.add(TextBuffer);

    this.stateStore = new StateStore('AtomEnvironments', 1);

    this.config.setSchema(null, {
      type: 'object',
      properties: _.clone(ConfigSchema)
    });

    this.keymaps = new KeymapManager({
      notificationManager: this.notifications
    });

    this.tooltips = new TooltipManager({
      keymapManager: this.keymaps,
      viewRegistry: this.views
    });

    this.grammars = new GrammarRegistry({ config: this.config });

    this.project = new Project({
      notificationManager: this.notifications,
      grammarRegistry: this.grammars,
      config: this.config,
      applicationDelegate: this.applicationDelegate
    });

    this.textEditors = new TextEditorRegistry({
      config: this.config,
      grammarRegistry: this.grammars,
      assert: this.assert.bind(this)
    });

    this.workspace = new Workspace({
      config: this.config,
      project: this.project,
      grammarRegistry: this.grammars,
      deserializerManager: this.deserializers,
      notificationManager: this.notifications,
      applicationDelegate: this.applicationDelegate,
      viewRegistry: this.views,
      assert: this.assert.bind(this),
      textEditorRegistry: this.textEditors,
      styleManager: this.styles,
      enablePersistence: this.enablePersistence
    });

    registerDefaultCommands({
      commandRegistry: this.commands,
      config: this.config,
      notificationManager: this.notifications,
      project: this.project,
      clipboard: this.clipboard
    });

    this.windowEventHandler = new WindowEventHandler({
      atomEnvironment: this,
      applicationDelegate: this.applicationDelegate
    });
  }

  initialize() {
    TextEditor.setClipboard(this.clipboard);
    TextEditor.setScheduler(this.views);
    // this will register the custom element, even before opening a buffer;
    require('./editor/text-editor-element');

    this.window = params.window;
    this.document = params.document;

    const { resourcePath } = this.applicationDelegate.getWindowLoadSettings();

    this.keymaps.resourcePath = resourcePath;
    this.keymaps.loadBundledKeymaps();

    this.commands.attach(this.window);

    this.initialStyleElements = this.styles.getSnapshot();
    this.document.body.classList.add(`platform-${process.platform}`);
    this.stylesElement = this.styles.buildStylesElement();
    this.document.head.appendChild(this.stylesElement);
    const didChangeStyles = this.didChangeStyles.bind(this);
    this.disposables.add(this.styles.onDidAddStyleElement(didChangeStyles));
    this.disposables.add(this.styles.onDidUpdateStyleElement(didChangeStyles));
    this.disposables.add(this.styles.onDidRemoveStyleElement(didChangeStyles));

    this.attachSaveStateListeners();

    this.windowEventHandler.initialize(this.window, this.document);

    this.workspace.initialize();
  }

  destroy() {
    if (!this.project) return;

    this.disposables.dispose();
    if (this.workspace) this.workspace.destroy();
    this.workspace = null;
    if (this.project) this.project.destroy();
    this.project = null;
    this.commands.clear();
    if (this.stylesElement) this.stylesElement.remove();
    if (this.windowEventHandler)
      this.windowEventHandler.unsubscribe();
    this.windowEventHandler = null;
  }

  /*
  Section: Private
  */

  assert(condition, message, callbackOrMetadata) {
    if (condition) return true;

    const error = new Error(`Assertion failed: ${message}`);
    Error.captureStackTrace(error, this.assert);

    if (callbackOrMetadata) {
      if (typeof callbackOrMetadata === 'function') {
        callbackOrMetadata(error);
      } else {
        error.metadata = callbackOrMetadata;
      }
    }

    if (!this.isReleasedVersion()) throw error;

    return false;
  }

  attachSaveStateListeners() {
    const saveState = _.debounce(() => {
      this.window.requestIdleCallback(() => {
        if (!this.unloading) this.saveState({ isUnloading: false });
      });
    }, this.saveStateDebounceInterval);
    this.document.addEventListener('mousedown', saveState, true);
    this.document.addEventListener('keydown', saveState, true);
    this.disposables.add(
      new Disposable(() => {
        this.document.removeEventListener('mousedown', saveState, true);
        this.document.removeEventListener('keydown', saveState, true);
      })
    );
  }

  async saveState(options, storageKey) {
    if (this.enablePersistence && this.project) {
      const state = this.serialize(options);
      if (!storageKey)
        storageKey = this.getStateKey(this.project && this.project.getPaths());
      if (storageKey) {
        await this.stateStore.save(storageKey, state);
      } else {
        this.applicationDelegate.setTemporaryWindowState(state);
      }
    }
  }
}

// stat 1608
//   require('util').promisify(fs.stat);
// crypto 1501
//   require('crypto')
// registerDefaultTargetForKeymaps:
//   this.keymaps.defaultTarget = this.workspace.getElement();
// if initialProjectRoots is empty show projects list;

module.exports = AtomEnvironment;
