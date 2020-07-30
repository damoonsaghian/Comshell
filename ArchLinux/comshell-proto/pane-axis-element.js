const { CompositeDisposable } = require('event-kit');
const PaneResizeHandleElement = require('./pane-resize-handle-element');

class PaneResizeHandleElement_ extends HTMLElement {
  createdCallback() {
    this.resizePane = this.resizePane.bind(this);
    this.resizeStopped = this.resizeStopped.bind(this);
    this.subscribeToDOMEvents();
  }

  subscribeToDOMEvents() {
    this.addEventListener('dblclick', this.resizeToFitContent.bind(this));
    this.addEventListener('mousedown', this.resizeStarted.bind(this));
  }

  attachedCallback() {
    // For some reason Chromium 58 is firing the attached callback after the
    // element has been detached, so we ignore the callback when a parent element
    // can't be found.
    if (this.parentElement) {
      this.isHorizontal = this.parentElement.classList.contains("horizontal");
      this.classList.add(this.isHorizontal ? 'horizontal' : 'vertical');
    }
  }

  detachedCallback() { this.resizeStopped() }

  resizeToFitContent() {
    // clear flex-grow css style of both pane
    this.previousSibling?.model.setFlexScale(1);
    this.nextSibling?.model.setFlexScale(1);
  }

  resizeStarted(e) {
    e.stopPropagation();
    if (!this.overlay) {
      this.overlay = document.createElement('div');
      this.overlay.classList.add('atom-pane-cursor-overlay');
      this.overlay.classList.add(this.isHorizontal ? 'horizontal' : 'vertical');
      this.appendChild(this.overlay);
    }
    document.addEventListener('mousemove', this.resizePane);
    document.addEventListener('mouseup', this.resizeStopped);
  }

  resizeStopped() {
    document.removeEventListener('mousemove', this.resizePane);
    document.removeEventListener('mouseup', this.resizeStopped);
    if (this.overlay) {
      this.removeChild(this.overlay);
      this.overlay = undefined;
    }
  }

  calcRatio(ratio1, ratio2, total) {
    const allRatio = ratio1 + ratio2;
    return [total * ratio1 / allRatio, total * ratio2 / allRatio];
  }

  setFlexGrow(prevSize, nextSize) {
    this.prevModel = this.previousSibling.model;
    this.nextModel = this.nextSibling.model;
    const totalScale = this.prevModel.getFlexScale() + this.nextModel.getFlexScale();
    const flexGrows = this.calcRatio(prevSize, nextSize, totalScale);
    this.prevModel.setFlexScale(flexGrows[0]);
    this.nextModel.setFlexScale(flexGrows[1]);
  }

  fixInRange(val, minValue, maxValue) {
    return Math.min(Math.max(val, minValue), maxValue);
  }

  resizePane({clientX, clientY, which}) {
    if (which !== 1) { return this.resizeStopped(); }
    if ((this.previousSibling == null) || (this.nextSibling == null)) {
      return this.resizeStopped();
    }

    let totalWidth, leftWidth, rightWidth, totalHeight, topHeight, bottomHeight;
    if (this.isHorizontal) {
      totalWidth = this.previousSibling.clientWidth + this.nextSibling.clientWidth;
      // get the left and right width after move the resize view
      leftWidth = clientX - this.previousSibling.getBoundingClientRect().left;
      leftWidth = this.fixInRange(leftWidth, 0, totalWidth);
      rightWidth = totalWidth - leftWidth;
      // set the flex grow by the ratio of left width and right width
      // to change pane width
      this.setFlexGrow(leftWidth, rightWidth);
    } else {
      totalHeight = this.previousSibling.clientHeight + this.nextSibling.clientHeight;
      topHeight = clientY - this.previousSibling.getBoundingClientRect().top;
      topHeight = this.fixInRange(topHeight, 0, totalHeight);
      bottomHeight = totalHeight - topHeight;
      this.setFlexGrow(topHeight, bottomHeight);
    }
  }
}

const PaneResizeHandleElement = document.registerElement(
  'atom-pane-resize-handle',
  { prototype: PaneResizeHandleElement_.prototype }
);

class PaneAxisElement extends HTMLElement {
  attachedCallback() {
    if (this.subscriptions == null)
      this.subscriptions = this.subscribeToModel();
    this.model.getChildren().forEach(
      (child, index) => this.childAdded({child, index})
    );
  }

  detachedCallback() {
    this.subscriptions.dispose();
    this.subscriptions = null;
    for (const child of this.model.getChildren()) {
      this.childRemoved({ child })
    }
  }

  initialize(model, viewRegistry) {
    this.model = model;
    this.viewRegistry = viewRegistry;
    if (this.subscriptions == null)
      this.subscriptions = this.subscribeToModel();
    this.model.getChildren().forEach(
      (child, index) => this.childAdded({child, index})
    );

    switch (this.model.getOrientation()) {
      case 'horizontal':
        this.classList.add('horizontal', 'pane-row');
        break;
      case 'vertical':
        this.classList.add('vertical', 'pane-column');
    }
    return this;
  }

  subscribeToModel() {)
    const subscriptions = new CompositeDisposable;
    subscriptions.add(this.model.onDidAddChild(
      this.childAdded.bind(this)
    ));
    subscriptions.add(this.model.onDidRemoveChild(
      this.childRemoved.bind(this)
    ));
    subscriptions.add(this.model.onDidReplaceChild(
      this.childReplaced.bind(this)
    ));
    subscriptions.add(this.model.observeFlexScale(
      this.flexScaleChanged.bind(this)
    ));
    return subscriptions;
  }

  isPaneResizeHandleElement(element) {
    return element?.nodeName.toLowerCase() === 'atom-pane-resize-handle';
  }

  childAdded({child, index}) {
    const view = this.viewRegistry.getView(child);
    this.insertBefore(view, this.children[index * 2]);

    let resizeHandle;
    const prevElement = view.previousSibling;
    // if previous element is not pane resize element, then insert new resize element
    if ((typeof prevElement !== "undefined" && prevElement !== null) && !this.isPaneResizeHandleElement(prevElement))
    {
      resizeHandle = document.createElement('atom-pane-resize-handle');
      this.insertBefore(resizeHandle, view);
    }

    const nextElement = view.nextSibling;
    // if next element isnot resize element, then insert new resize element
    if ((typeof nextElement !== "undefined" && nextElement !== null) && !this.isPaneResizeHandleElement(nextElement))
    {
      resizeHandle = document.createElement('atom-pane-resize-handle');
      this.insertBefore(resizeHandle, nextElement);
    }
  }

  childRemoved({ child }) {
    const view = this.viewRegistry.getView(child);
    const siblingView = view.previousSibling;
    // make sure next sibling view is pane resize view
    if ((typeof siblingView !== "undefined" && siblingView !== null) &&
      this.isPaneResizeHandleElement(siblingView))
    {
      siblingView.remove();
    }
    view.remove();
  }

  childReplaced({index, oldChild, newChild}) {
    const focusedElement = this.hasFocus() ? document.activeElement : undefined;
    this.childRemoved({ child: oldChild, index });
    this.childAdded({ child: newChild, index });
    if (document.activeElement === document.body)
      focusedElement?.focus();
  }

  flexScaleChanged(flexScale) { this.style.flexGrow = flexScale; }

  hasFocus() {
    return this === document.activeElement || this.contains(document.activeElement);
  }
}

module.exports = PaneAxisElement =
  document.registerElement('atom-pane-axis', { prototype: PaneAxisElement.prototype });
