const fs = require('fs-plus');
const path = require('path');
const KeymapManager = require('atom-keymap');

KeymapManager.prototype.onDidLoadBundledKeymaps = function (callback) {
  this.emitter.on('did-load-bundled-keymaps', callback);
};

KeymapManager.prototype.onDidLoadUserKeymap = function (callback) {
  this.emitter.on('did-load-user-keymap', callback);
};

KeymapManager.prototype.canLoadBundledKeymapsFromMemory = () => false;

KeymapManager.prototype.loadBundledKeymaps = function () {
  keymapsPath = path.join(this.resourcePath, 'keymaps');
  this.loadKeymap(keymapsPath);
  this.emitter.emit('did-load-bundled-keymaps');
};

KeymapManager.prototype.getUserKeymapPath = function () {};
KeymapManager.prototype.loadUserKeymap = function () {};
KeymapManager.prototype.subscribeToFileReadFailure = function () {};

module.exports = KeymapManager;
