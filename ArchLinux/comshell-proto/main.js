const url = require('url');
const path = require('path');
const fs = require('fs-plus');
const _ = require('underscore-plus');
const AtomEnvironment = require('./atom-environment');
const ApplicationDelegate = require('./application-delegate');
const Clipboard = require('./clipboard');
const TextEditor = require('./editor/text-editor');

const win = nw.Window.get();
win.maximize();

process.env.ATOM_HOME = path.join(process.env.HOME, '.atom');

// make React faster
if (process.env.NODE_ENV == null) {
  process.env.NODE_ENV = 'production';
}

const clipboard = new Clipboard();
TextEditor.setClipboard(clipboard);

global.atom = new AtomEnvironment({
  clipboard,
  applicationDelegate: new ApplicationDelegate(),
  enablePersistence: true
});

TextEditor.setScheduler(global.atom.views);

win.on('close', async () => {
  if (await atom.prepareToUnloadEditorWindow())
    win.close(true);
});

global.atom.initialize({ window, document });

global.atom.startEditorWindow().then(function () {
  // workaround for focus getting cleared upon window creation;
  const windowFocused = function () {
    window.removeEventListener('focus', windowFocused);
    setTimeout(() => document.querySelector('atom-workspace').focus(), 0);
  };
  window.addEventListener('focus', windowFocused);
});
