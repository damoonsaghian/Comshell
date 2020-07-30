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
const URIHandlerRegistry = require('./uri-handler-registry');
const CoreURIHandlers = require('./core-uri-handlers');
const StyleManager = require('./style-manager');
const ThemeManager = require('./theme-manager');
const Project = require('./project');
const Workspace = require('./workspace');
const PaneContainer = require('./pane-container');
const PaneAxis = require('./pane-axis');
const Pane = require('./pane');
const Dock = require('./dock');
const GrammarRegistry = require('./editor/grammar-registry');
const TextEditorRegistry = require('./editor/text-editor-registry');
const TextEditor = require('./editor/text-editor');

const TextBuffer = require('text-buffer');

require('util').promisify(fs.stat);
