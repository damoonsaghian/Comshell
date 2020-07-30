const {Emitter, CompositeDisposable} = require('event-kit');

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

module.exports = StylesElement = document.registerElement('atom-styles', {
  prototype: StylesElement.prototype
});
