const { Emitter, Disposable } = require('event-kit');

module.exports = class ApplicationDelegate {
  constructor() {
    this.fileRecoveryService = new FileRecoveryService(
      require('path').join(process.env.ATOM_HOME, 'recovery')
    );
  }

  getWindowLoadSettings() {
    return {
      appName: 'Comshell',
      resourcePath: __dirname,
      atomHome: process.env.ATOM_HOME,
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
};
