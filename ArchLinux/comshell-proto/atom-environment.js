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
const StyleManager = require('./style/style-manager');
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
  keymapsPath = path.join(__dirname, 'keymaps.json');
  this.loadKeymap(keymapsPath);
  this.emitter.emit('did-load-bundled-keymaps');
};

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
      config: this.config
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
      atomEnvironment: this
    });
  }

  initialize() {
    TextEditor.setClipboard(this.clipboard);
    TextEditor.setScheduler(this.views);
    // this will register the custom element, even before opening a buffer;
    require('./editor/text-editor-element');

    this.window = params.window;
    this.document = params.document;

    this.keymaps.loadBundledKeymaps();

    this.commands.attach(this.window);

    this.styles.applyStylesheets();
    this.initialStyleElements = this.styles.getSnapshot();
    this.document.body.classList.add(`platform-${process.platform}`);
    this.stylesElement = this.styles.buildStylesElement();
    this.document.head.appendChild(this.stylesElement);
    const didChangeStyles = (styleElement) => {
      TextEditor.didUpdateStyles();
      if (styleElement.textContent.indexOf('scrollbar') >= 0)
        TextEditor.didUpdateScrollbarStyles();
    }
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

  // Extended: Move current window to the center of the screen.
  center() {
    nw.Window.get().setPosition('center');
    return Promise(resolve => resolve());
  }

  // Extended: Focus the current window.
  focus() {
    nw.Window.get().focus();
    return Promise(resolve => resolve());
  }

  // Extended: Show the current window.
  show() {
    nw.Window.get().show();
    return Promise(resolve => resolve());
  }

  // Extended: Hide the current window.
  hide() {
    nw.Window.get().hide();
    return Promise(resolve => resolve());
  }

  // Extended: Returns a {Boolean} that is `true` if the current window is in full screen mode.
  isFullScreen() {
    return nw.Window.get().isFullscreen();
  }

  // Extended: Set the full screen state of the current window.
  setFullScreen(fullScreen = false) {
    nw.Window.get().enterFullscreen();
    return Promise(resolve => resolve());
  }

  // establishing a real application window;
  async startEditorWindow() {
    // await this.stateStore.clear();

    this.unloading = false;

    const loadStatePromise = this.loadState().then(async state => {
      this.keymaps.defaultTarget = this.workspace.getElement();

      await this.deserialize(state);

      this.document.body.appendChild(this.workspace.getElement());

      // https://github.com/atom/command-palette
      // https://github.com/atom/snippets
      // https://github.com/atom/spell-check
      // https://github.com/atom/image-view
      // https://github.com/atom/autocomplete-plus
      // https://github.com/atom/autocomplete-css
      // https://github.com/atom/bracket-matcher
      // https://github.com/atom/wrap-guide
      // https://github.com/atom/atom/tree/master/packages/link
      // https://github.com/atom/language-hyperlink
      // https://github.com/atom/language-json
      // https://github.com/atom/language-yaml
      // https://github.com/atom/language-css
      // https://github.com/atom/language-less
      // https://github.com/atom/image-view
      // https://github.com/atom/atom/tree/master/packages/autoflow
      // https://github.com/atom/fuzzy-finder
      // https://github.com/atom/atom/tree/master/packages/go-to-line
      // https://github.com/atom/whitespace
      // https://github.com/akonwi/git-plus
      // https://github.com/mauricioszabo/simple-git
      // https://github.com/atom/atom/tree/master/packages/git-diff
      // https://github.com/atom/language-git
      // https://github.com/atom/atom/tree/master/packages/language-rust-bundled
      // https://github.com/atom/language-toml
      // https://github.com/atom/language-shellscript
      // https://github.com/atom/language-python
      // https://github.com/atom/language-c
      // https://github.com/atom/language-make
      // https://github.com/atom/language-property-list

      // if initialProjectRoots is empty show projects list;
    });

    const output = await Promise.all([loadStatePromise]);
    return output;
  }

  async prepareToUnloadEditorWindow() {
    try {
      await this.saveState({ isUnloading: true });
    } catch (error) {
      console.error(error);
    }

    const closing =
      !this.workspace ||
      (await this.workspace.confirmClose({
        windowCloseRequested: true,
        projectHasPaths: this.project.getPaths().length > 0
      }));

    if (closing)
      this.unloading = true;
    return closing;
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
        nw.Window.get().temporaryState = state;
      }
    }
  }

  loadState(stateKey) {
    const args = nw.App.fullArgv;
    const userDataDir = args[args.indexOf('--user-data-dir') + 1]; // nw.App.dataPath
    const projectRootPath = userDataDir ? path.join(userDataDir, '../..') : null;
    const initialProjectRoots = projectRootPath ? [projectRootPath] : null;

    if (this.enablePersistence) {
      if (!stateKey)
        stateKey = this.getStateKey(initialProjectRoots);
      if (stateKey) {
        return this.stateStore.load(stateKey);
      } else {
        return nw.Window.get().temporaryState;
      }
    } else {
      return Promise.resolve(null);
    }
  }

  serialize(options) {
    return {
      project: this.project.serialize(options),
      workspace: this.workspace.serialize(),
      grammars: this.grammars.serialize(),
      fullScreen: this.isFullScreen()
    };
  }

  async deserialize(state) {
    if (!state) return Promise.resolve();

    this.setFullScreen(state.fullScreen);

    if (state.project) {
      try {
        await this.project.deserialize(state.project, this.deserializers);
      } catch (error) {
        // We handle the missingProjectPaths case in openLocations().
        if (!error.missingProjectPaths) {
          this.notifications.addError('Unable to deserialize project', {
            description: error.message,
            stack: error.stack
          });
        }
      }
    }

    if (state.grammars) this.grammars.deserialize(state.grammars);

    if (state.workspace)
      this.workspace.deserialize(state.workspace, this.deserializers);
  }

  getStateKey(paths) {
    if (paths && paths.length > 0) {
      const sha1 = require('crypto')
        .createHash('sha1')
        .update(
          paths
            .slice()
            .sort()
            .join('\n')
        )
        .digest('hex');
      return `editor-${sha1}`;
    } else {
      return null;
    }
  }

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

    return false;
  }
}

module.exports = AtomEnvironment;
