const path = require('path');
const AtomEnvironment = require('./atom-environment');

const win = nw.Window.get();
win.maximize();

// for each user data directory, there can only be one instance of NW app;
// if we try to open a second instance, the first one will be notified,
//   and will focus its window;
nw.App.on('open', _args => win.focus());

process.env.ATOM_HOME = path.join(process.env.HOME, '.atom');

// make React faster
if (process.env.NODE_ENV == null) {
  process.env.NODE_ENV = 'production';
}

global.atom = new AtomEnvironment();
global.atom.initialize({ window, document });
global.atom.startEditorWindow().then(function () {
  // workaround for focus getting cleared upon window creation;
  const windowFocused = function () {
    window.removeEventListener('focus', windowFocused);
    setTimeout(() => document.querySelector('atom-workspace').focus(), 0);
  };
  window.addEventListener('focus', windowFocused);
});

win.on('close', async () => {
  if (await global.atom.prepareToUnloadEditorWindow())
    win.close(true);
});
