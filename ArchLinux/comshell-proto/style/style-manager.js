const { Emitter, Disposable, CompositeDisposable } = require('event-kit');
const crypto = require('crypto');
const fs = require('fs-plus');
const path = require('path');
const postcss = require('postcss');
const selectorParser = require('postcss-selector-parser');
const LessCache = require ('less-cache');

class StylesElement extends HTMLElement {
  subscriptions = null;
  context = null;

  onDidAddStyleElement(callback) {
    this.emitter.on('did-add-style-element', callback);
  }

  onDidRemoveStyleElement(callback) {
    this.emitter.on('did-remove-style-element', callback);
  }

  onDidUpdateStyleElement(callback) {
    this.emitter.on('did-update-style-element', callback);
  }

  createdCallback() {
    this.subscriptions = new CompositeDisposable();
    this.emitter = new Emitter();
    this.styleElementClonesByOriginalElement = new WeakMap();
  }

  attachedCallback() {
    this.context = this.getAttribute('context');
    if (this.context === null) { this.context = undefined; }
  }

  detachedCallback() {
    this.subscriptions.dispose();
    this.subscriptions = new CompositeDisposable();
  }

  attributeChangedCallback(attrName, oldVal, newVal) {
    if (attrName === 'context') this.contextChanged();
  }

  initialize(styleManager) {
    this.styleManager = styleManager;
    if (this.styleManager == null) {
      throw new Error("Must pass a styleManager parameter when initializing a StylesElement");
    }

    this.subscriptions.add(
      this.styleManager.observeStyleElements(this.styleElementAdded.bind(this))
    );
    this.subscriptions.add(
      this.styleManager.onDidRemoveStyleElement(this.styleElementRemoved.bind(this))
    );
    this.subscriptions.add(
      this.styleManager.onDidUpdateStyleElement(this.styleElementUpdated.bind(this))
    );
  }

  contextChanged() {
    if (this.subscriptions == null) { return; }

    for (const child of Array.prototype.slice.call(this.children)) {
      this.styleElementRemoved(child);
    }
    this.context = this.getAttribute('context');
    for (const styleElement of this.styleManager.getStyleElements()) {
      this.styleElementAdded(styleElement);
    }
  }

  styleElementAdded(styleElement) {
    if (!this.styleElementMatchesContext(styleElement)) { return; }

    const styleElementClone = styleElement.cloneNode(true);
    styleElementClone.sourcePath = styleElement.sourcePath;
    styleElementClone.context = styleElement.context;
    styleElementClone.priority = styleElement.priority;
    this.styleElementClonesByOriginalElement.set(styleElement, styleElementClone);

    const priority = styleElement.priority;
    let insertBefore;
    if (typeof priority !== "undefined" && priority !== null) {
      for (child of this.children) {
        if (child.priority > priority) {
          insertBefore = child;
          break;
        }
      }
    }

    this.insertBefore(styleElementClone, insertBefore);
    this.emitter.emit('did-add-style-element', styleElementClone);
  }

  styleElementRemoved(styleElement) {
    if (!this.styleElementMatchesContext(styleElement)) { return; }

    let styleElementClone = this.styleElementClonesByOriginalElement.get(styleElement);
    if (styleElementClone == null) { styleElementClone = styleElement; }
    styleElementClone.remove();
    this.emitter.emit('did-remove-style-element', styleElementClone);
  }

  styleElementUpdated(styleElement) {
    if (!this.styleElementMatchesContext(styleElement)) { return; }

    const styleElementClone = this.styleElementClonesByOriginalElement.get(styleElement);
    styleElementClone.textContent = styleElement.textContent;
    this.emitter.emit('did-update-style-element', styleElementClone);
  }

  styleElementMatchesContext(styleElement) {
    return (this.context == null) || (styleElement.context === this.context);
  }
}

window.customElements.define('atom-styles', StylesElement);

// Extended: A singleton instance of this class available via `atom.styles`,
// which you can use to globally query and observe the set of active style
// sheets. The `StyleManager` doesn't add any style elements to the DOM on its
// own, but is instead subscribed to by individual `<atom-styles>` elements,
// which clone and attach style elements in different contexts.
module.exports = class StyleManager {
  constructor() {
    this.emitter = new Emitter();
    this.styleElements = [];
    this.styleElementsBySourcePath = {};
    this.deprecationsBySourcePath = {};
  }

  loadLessStylesheet(lessStylesheetPath) {
    if (this.lessCache == null) {
      this.lessCache = new LessCache({
        importPaths: [
          fs.resolveOnLoadPath('./base/'),
          fs.resolveOnLoadPath('./one-light-ui/'),
          fs.resolveOnLoadPath('./one-light-syntax/')
        ],
        cacheDir: path.join(process.env.ATOM_HOME, 'compile-cache', 'less'),
        fallbackDir: path.join(resourcePath, 'less-compile-cache')
      })
    }

    try {
      return this.lessCache.readFileSync(lessStylesheetPath);
    } catch (error) {
      error.less = true;
      throw error;
    }
  }

  loadStylesheet(stylesheetPath) {
    if (path.extname(stylesheetPath) === '.less') {
      return this.loadLessStylesheet(stylesheetPath);
    } else {
      return fs.readFileSync(stylesheetPath, 'utf8');
    }
  }

  requireStylesheet(stylesheetPath, priority) {
    let fullPath;
    if (path.extname(stylesheetPath).length > 0) {
      fullPath = fs.resolveOnLoadPath(stylesheetPath);
    } else {
      fullPath = fs.resolveOnLoadPath(stylesheetPath, ['css', 'less']);
    }

    if (fullPath) {
      const content = this.loadStylesheet(fullPath);
      this.addStyleSheet(content, { sourcePath: fullPath, priority });
    } else {
      throw new Error(`could not find a file at path '${stylesheetPath}'`);
    }
  }

  applyStylesheets() {
    this.requireStylesheet('./base/index', -2);
    this.requireStylesheet('./one-light-ui/index', 0);
    this.requireStylesheet('./one-light-syntax/index', 0);
  }

  /*
  Section: Event Subscription
  */

  // Extended: Invoke `callback` for all current and future style elements.
  //
  // * `callback` {Function} that is called with style elements.
  //   * `styleElement` An `HTMLStyleElement` instance. The `.sheet` property
  //     will be null because this element isn't attached to the DOM. If you want
  //     to attach this element to the DOM, be sure to clone it first by calling
  //     `.cloneNode(true)` on it. The style element will also have the following
  //     non-standard properties:
  //     * `sourcePath` A {String} containing the path from which the style
  //       element was loaded.
  //     * `context` A {String} indicating the target context of the style
  //       element.
  //
  // Returns a {Disposable} on which `.dispose()` can be called to cancel the
  // subscription.
  observeStyleElements(callback) {
    for (let styleElement of this.getStyleElements()) {
      callback(styleElement);
    }

    return this.onDidAddStyleElement(callback);
  }

  // Extended: Invoke `callback` when a style element is added.
  //
  // * `callback` {Function} that is called with style elements.
  //   * `styleElement` An `HTMLStyleElement` instance. The `.sheet` property
  //     will be null because this element isn't attached to the DOM. If you want
  //     to attach this element to the DOM, be sure to clone it first by calling
  //     `.cloneNode(true)` on it. The style element will also have the following
  //     non-standard properties:
  //     * `sourcePath` A {String} containing the path from which the style
  //       element was loaded.
  //     * `context` A {String} indicating the target context of the style
  //       element.
  //
  // Returns a {Disposable} on which `.dispose()` can be called to cancel the
  // subscription.
  onDidAddStyleElement(callback) {
    return this.emitter.on('did-add-style-element', callback);
  }

  // Extended: Invoke `callback` when a style element is removed.
  //
  // * `callback` {Function} that is called with style elements.
  //   * `styleElement` An `HTMLStyleElement` instance.
  //
  // Returns a {Disposable} on which `.dispose()` can be called to cancel the
  // subscription.
  onDidRemoveStyleElement(callback) {
    return this.emitter.on('did-remove-style-element', callback);
  }

  // Extended: Invoke `callback` when an existing style element is updated.
  //
  // * `callback` {Function} that is called with style elements.
  //   * `styleElement` An `HTMLStyleElement` instance. The `.sheet` property
  //      will be null because this element isn't attached to the DOM. The style
  //      element will also have the following non-standard properties:
  //     * `sourcePath` A {String} containing the path from which the style
  //       element was loaded.
  //     * `context` A {String} indicating the target context of the style
  //       element.
  //
  // Returns a {Disposable} on which `.dispose()` can be called to cancel the
  // subscription.
  onDidUpdateStyleElement(callback) {
    return this.emitter.on('did-update-style-element', callback);
  }

  onDidUpdateDeprecations(callback) {
    return this.emitter.on('did-update-deprecations', callback);
  }

  /*
  Section: Reading Style Elements
  */

  // Extended: Get all loaded style elements.
  getStyleElements() {
    return this.styleElements.slice();
  }

  addStyleSheet(source, params = {}) {
    let styleElement;
    let updated;
    if (
      params.sourcePath != null &&
      this.styleElementsBySourcePath[params.sourcePath] != null
    ) {
      updated = true;
      styleElement = this.styleElementsBySourcePath[params.sourcePath];
    } else {
      updated = false;
      styleElement = document.createElement('style');
      if (params.sourcePath != null) {
        styleElement.sourcePath = params.sourcePath;
        styleElement.setAttribute('source-path', params.sourcePath);
      }
      if (params.context != null) {
        styleElement.context = params.context;
        styleElement.setAttribute('context', params.context);
      }
      if (params.priority != null) {
        styleElement.priority = params.priority;
        styleElement.setAttribute('priority', params.priority);
      }
    }

    styleElement.textContent = source;

    if (updated) {
      this.emitter.emit('did-update-style-element', styleElement);
    } else {
      this.addStyleElement(styleElement);
    }
    return new Disposable(() => {
      this.removeStyleElement(styleElement);
    });
  }

  addStyleElement(styleElement) {
    let insertIndex = this.styleElements.length;
    if (styleElement.priority != null) {
      for (let i = 0; i < this.styleElements.length; i++) {
        const existingElement = this.styleElements[i];
        if (existingElement.priority > styleElement.priority) {
          insertIndex = i;
          break;
        }
      }
    }

    this.styleElements.splice(insertIndex, 0, styleElement);
    if (
      styleElement.sourcePath != null &&
      this.styleElementsBySourcePath[styleElement.sourcePath] == null
    ) {
      this.styleElementsBySourcePath[styleElement.sourcePath] = styleElement;
    }
    this.emitter.emit('did-add-style-element', styleElement);
  }

  removeStyleElement(styleElement) {
    const index = this.styleElements.indexOf(styleElement);
    if (index !== -1) {
      this.styleElements.splice(index, 1);
      if (styleElement.sourcePath != null) {
        delete this.styleElementsBySourcePath[styleElement.sourcePath];
      }
      this.emitter.emit('did-remove-style-element', styleElement);
    }
  }

  getDeprecations() {
    return this.deprecationsBySourcePath;
  }

  clearDeprecations() {
    this.deprecationsBySourcePath = {};
  }

  getSnapshot() {
    return this.styleElements.slice();
  }

  restoreSnapshot(styleElementsToRestore) {
    for (let styleElement of this.getStyleElements()) {
      if (!styleElementsToRestore.includes(styleElement)) {
        this.removeStyleElement(styleElement);
      }
    }

    const existingStyleElements = this.getStyleElements();
    for (let styleElement of styleElementsToRestore) {
      if (!existingStyleElements.includes(styleElement)) {
        this.addStyleElement(styleElement);
      }
    }
  }

  buildStylesElement() {
    var stylesElement = new StylesElement();
    stylesElement.initialize(this);
    return stylesElement;
  }
};
