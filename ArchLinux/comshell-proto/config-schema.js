// this is loaded by atom-environment.js;
// see "https://atom.io/docs/api/latest/Config" for more information about config schemas;
const configSchema = {
  core: {
    type: 'object',
    properties: {
      ignoredNames: {
        type: 'array',
        default: [
          '.*',
          '*.lock'
        ],
        items: {
          type: 'string'
        },
        description:
          'List of [glob patterns](https://en.wikipedia.org/wiki/Glob_%28programming%29). Files and directories matching these patterns will be ignored by some packages, such as the fuzzy finder and tree view. Individual packages might have additional config settings for ignoring names.'
      },
      excludeVcsIgnoredPaths: {
        type: 'boolean',
        default: true,
        title: 'Exclude VCS Ignored Paths',
        description:
          "Files and directories ignored by the current project's VCS will be ignored by some packages, such as the fuzzy finder and find and replace. For example, projects using Git have these paths defined in the .gitignore file. Individual packages might have additional config settings for ignoring VCS ignored files and folders."
      },
      followSymlinks: {
        type: 'boolean',
        default: true,
        description:
          'Follow symbolic links when searching files and when opening files with the fuzzy finder.'
      },
      customFileTypes: {
        type: 'object',
        default: {},
        description:
          'Associates scope names (e.g. `"source.js"`) with arrays of file extensions and file names (e.g. `["Somefile", ".js2"]`)',
        additionalProperties: {
          type: 'array',
          items: {
            type: 'string'
          }
        }
      },
      closeDeletedFileTabs: {
        type: 'boolean',
        default: false,
        title: 'Close Deleted File Tabs',
        description:
          'Close corresponding editors when a file is deleted outside Atom.'
      },
      destroyEmptyPanes: {
        type: 'boolean',
        default: true,
        title: 'Remove Empty Panes',
        description:
          'When the last tab of a pane is closed, remove that pane as well.'
      },
      closeEmptyWindows: {
        type: 'boolean',
        default: true,
        description:
          "When a window with no open tabs or panes is given the 'Close Tab' command, close that window."
      },
      fileEncoding: {
        description:
          'Default character set encoding to use when reading and writing files.',
        type: 'string',
        default: 'utf8',
        enum: [
          {
            value: 'utf8',
            description: 'Unicode (UTF-8)'
          },
          {
            value: 'utf16le',
            description: 'Unicode (UTF-16 LE)'
          },
          {
            value: 'utf16be',
            description: 'Unicode (UTF-16 BE)'
          }
        ]
      },
      restorePreviousWindowsOnStart: {
        type: 'string',
        enum: ['no', 'yes', 'always'],
        default: 'yes',
        description:
          "When selected 'no', a blank environment is loaded. When selected 'yes' and Atom is started from the icon or `atom` by itself from the command line, restores the last state of all Atom windows; otherwise a blank environment is loaded. When selected 'always', restores the last state of all Atom windows always, no matter how Atom is started."
      },
      allowPendingPaneItems: {
        description:
          'Allow items to be previewed without adding them to a pane permanently, such as when single clicking files in the tree view.',
        type: 'boolean',
        default: false
      },
      warnOnLargeFileLimit: {
        description:
          'Warn before opening files larger than this number of megabytes.',
        type: 'number',
        default: 40
      },
      fileSystemWatcher: {
        description:
          'Choose the underlying implementation used to watch for filesystem changes. Emulating changes will miss any events caused by applications other than Atom, but may help prevent crashes or freezes.',
        type: 'string',
        default: 'native',
        enum: [
          {
            value: 'native',
            description: 'Native operating system APIs'
          },
          {
            value: 'experimental',
            description: 'Experimental filesystem watching library'
          },
          {
            value: 'poll',
            description: 'Polling'
          },
          {
            value: 'atom',
            description: 'Emulated with Atom events'
          }
        ]
      },
      useTreeSitterParsers: {
        type: 'boolean',
        default: true,
        description: 'Use Tree-sitter parsers for supported languages.'
      }
    }
  },
  editor: {
    type: 'object',
    // These settings are used in scoped fashion only. No defaults.
    properties: {
      commentStart: {
        type: ['string', 'null']
      },
      commentEnd: {
        type: ['string', 'null']
      },
      increaseIndentPattern: {
        type: ['string', 'null']
      },
      decreaseIndentPattern: {
        type: ['string', 'null']
      },
      foldEndPattern: {
        type: ['string', 'null']
      },
      // These can be used as globals or scoped, thus defaults.
      fontFamily: {
        type: 'string',
        default: 'monospace',
        description: 'The name of the font family used for editor text.'
      },
      fontSize: {
        type: 'integer',
        default: 14,
        minimum: 1,
        maximum: 100,
        description: 'Height in pixels of editor text.'
      },
      lineHeight: {
        type: ['string', 'number'],
        default: 1.5,
        description: 'Height of editor lines, as a multiplier of font size.'
      },
      showCursorOnSelection: {
        type: 'boolean',
        default: true,
        description: 'Show cursor while there is a selection.'
      },
      showInvisibles: {
        type: 'boolean',
        default: false,
        description:
          'Render placeholders for invisible characters, such as tabs, spaces and newlines.'
      },
      showIndentGuide: {
        type: 'boolean',
        default: true,
        description: 'Show indentation indicators in the editor.'
      },
      showLineNumbers: {
        type: 'boolean',
        default: true,
        description: "Show line numbers in the editor's gutter."
      },
      atomicSoftTabs: {
        type: 'boolean',
        default: true,
        description:
          'Skip over tab-length runs of leading whitespace when moving the cursor.'
      },
      autoIndent: {
        type: 'boolean',
        default: true,
        description: 'Automatically indent the cursor when inserting a newline.'
      },
      autoIndentOnPaste: {
        type: 'boolean',
        default: true,
        description:
          'Automatically indent pasted text based on the indentation of the previous line.'
      },
      nonWordCharacters: {
        type: 'string',
        default: '/\\()"\':,.;<>~!@#$%^&*|+=[]{}`?-…',
        description:
          'A string of non-word characters to define word boundaries.'
      },
      preferredLineLength: {
        type: 'integer',
        default: 100,
        minimum: 1,
        description:
          'Identifies the length of a line which is used when wrapping text with the `Soft Wrap At Preferred Line Length` setting enabled, in number of characters.'
      },
      maxScreenLineLength: {
        type: 'integer',
        default: 500,
        minimum: 500,
        description:
          'Defines the maximum width of the editor window before soft wrapping is enforced, in number of characters.'
      },
      tabLength: {
        type: 'integer',
        default: 2,
        minimum: 1,
        description: 'Number of spaces used to represent a tab.'
      },
      softWrap: {
        type: 'boolean',
        default: true,
        description:
          'Wraps lines that exceed the width of the window. When `Soft Wrap At Preferred Line Length` is set, it will wrap to the number of characters defined by the `Preferred Line Length` setting.'
      },
      softTabs: {
        type: 'boolean',
        default: true,
        description:
          'If the `Tab Type` config setting is set to "auto" and autodetection of tab type from buffer content fails, then this config setting determines whether a soft tab or a hard tab will be inserted when the Tab key is pressed.'
      },
      tabType: {
        type: 'string',
        default: 'auto',
        enum: ['auto', 'soft', 'hard'],
        description:
          'Determine character inserted when Tab key is pressed. Possible values: "auto", "soft" and "hard". When set to "soft" or "hard", soft tabs (spaces) or hard tabs (tab characters) are used. When set to "auto", the editor auto-detects the tab type based on the contents of the buffer (it uses the first leading whitespace on a non-comment line), or uses the value of the Soft Tabs config setting if auto-detection fails.'
      },
      softWrapAtPreferredLineLength: {
        type: 'boolean',
        default: true,
        description:
          "Instead of wrapping lines to the window's width, wrap lines to the number of characters defined by the `Preferred Line Length` setting. This will only take effect when the soft wrap config setting is enabled globally or for the current language. **Note:** If you want to hide the wrap guide (the vertical line) you can disable the `wrap-guide` package."
      },
      softWrapHangingIndent: {
        type: 'integer',
        default: 2,
        minimum: 0,
        description:
          'When soft wrap is enabled, defines length of additional indentation applied to wrapped lines, in number of characters.'
      },
      scrollSensitivity: {
        type: 'integer',
        default: 40,
        minimum: 10,
        maximum: 200,
        description:
          'Determines how fast the editor scrolls when using a mouse or trackpad.'
      },
      scrollPastEnd: {
        type: 'boolean',
        default: false,
        description:
          'Allow the editor to be scrolled past the end of the last line.'
      },
      undoGroupingInterval: {
        type: 'integer',
        default: 300,
        minimum: 0,
        description:
          'Time interval in milliseconds within which text editing operations will be grouped together in the undo history.'
      },
      confirmCheckoutHeadRevision: {
        type: 'boolean',
        default: true,
        title: 'Confirm Checkout HEAD Revision',
        description:
          'Show confirmation dialog when checking out the HEAD revision and discarding changes to current file since last commit.'
      },
      invisibles: {
        type: 'object',
        description:
          'A hash of characters Atom will use to render whitespace characters. Keys are whitespace character types, values are rendered characters (use value false to turn off individual whitespace character types).',
        properties: {
          eol: {
            type: ['boolean', 'string'],
            default: '¬',
            maximumLength: 1,
            description:
              'Character used to render newline characters (\\n) when the `Show Invisibles` setting is enabled. '
          },
          space: {
            type: ['boolean', 'string'],
            default: '·',
            maximumLength: 1,
            description:
              'Character used to render leading and trailing space characters when the `Show Invisibles` setting is enabled.'
          },
          tab: {
            type: ['boolean', 'string'],
            default: '»',
            maximumLength: 1,
            description:
              'Character used to render hard tab characters (\\t) when the `Show Invisibles` setting is enabled.'
          },
          cr: {
            type: ['boolean', 'string'],
            default: '¤',
            maximumLength: 1,
            description:
              'Character used to render carriage return characters (for Microsoft-style line endings) when the `Show Invisibles` setting is enabled.'
          }
        }
      },
      zoomFontWhenCtrlScrolling: {
        type: 'boolean',
        default: false,
        description:
          'Change the editor font size when pressing the Ctrl key and scrolling the mouse up/down.'
      },
      multiCursorOnClick: {
        type: 'boolean',
        default: true,
        description:
          'Add multiple cursors when pressing the Ctrl key (Command key on MacOS) and clicking the editor.'
      }
    }
  }
};

module.exports = configSchema;
