const path = require('path');
const fs = require('fs-plus');
const { Disposable, CompositeDisposable } = require('event-kit');

// https://github.com/jspiro/tab-smart-sort
// https://github.com/xixixao/stacked-tabs
// https://github.com/ericcornelissen/pinned-tabs-for-atom
// https://github.com/paulpflug/foldername-tabs

function iconClassForPath(filePath) {
  if (typeof filePath !== 'string') return 'icon-file-text';

  const extension = path.extname(filePath);

  if (fs.isSymbolicLinkSync(filePath)) {
    return 'icon-file-symlink-file';
  } else if (fs.isReadmePath(filePath)) {
    return 'icon-book';
  } else if (fs.isCompressedExtension(extension)) {
    return 'icon-file-zip';
  } else if (fs.isImageExtension(extension)) {
    return 'icon-file-media';
  } else if (fs.isPdfExtension(extension)) {
    return 'icon-file-pdf';
  } else if (fs.isBinaryExtension(extension)) {
    return 'icon-file-binary';
  } else {
    return 'icon-file-text';
  }
}

class TabView {
  constructor({ item, pane, didClickCloseIcon, tabs, location }) {
    this.item = item;
    this.pane = pane;
    this.tabs = tabs;
    if (typeof this.item.getPath === 'function')
      this.path = this.item.getPath();

    this.element = document.createElement('li');
    this.element.setAttribute('is', 'tabs-tab');
    if (['TextEditor', 'TestView'].indexOf(this.item.constructor.name) > -1)
      this.element.classList.add('texteditor');
    this.element.classList.add('tab');

    this.itemTitle = document.createElement('div');
    this.itemTitle.classList.add('title');
    this.element.appendChild(this.itemTitle);

    if (
      location === 'center' ||
      !(typeof this.item.isPermanentDockItem === 'function' && this.item.isPermanentDockItem())
    ) {
      const closeIcon = document.createElement('div');
      closeIcon.classList.add('close-icon');
      closeIcon.onclick = didClickCloseIcon;
      this.element.appendChild(closeIcon);
    }

    this.subscriptions = new CompositeDisposable();

    this.handleEvents();
    this.updateDataAttributes();
    this.updateTitle();
    this.updateIcon();
    this.updateModifiedStatus();

    if (this.isItemPending()) {
      this.itemTitle.classList.add('temp');
      this.element.classList.add('pending-tab');
    }

    this.element.pane = this.pane;
    this.element.item = this.item;
    this.element.itemTitle = this.itemTitle;
    this.element.path = this.path;
  }

  handleEvents() {
    this.subscriptions.add(this.pane.onDidDestroy(
      () => this.destroy()
    ));
    this.subscriptions.add(this.pane.onItemDidTerminatePendingState(
      (item) => { if (item === this.item) { this.clearPending() } }
    ));

    if (typeof this.item.onDidChangeTitle === 'function') {
      onDidChangeTitleDisposable = this.item.onDidChangeTitle(() => this.updateTitle());
      if (Disposable.isDisposable(onDidChangeTitleDisposable)) {
        this.subscriptions.add(onDidChangeTitleDisposable);
      } else {
        console.warn("::onDidChangeTitle does not return a valid Disposable!", this.item);
      }
    } else if (typeof this.item.on === 'function') {
      //TODO Remove once old events are no longer supported
      this.item.on('title-changed', () => this.updateTitle());
      this.subscriptions.add({ dispose: () => {
        if (typeof this.item.off === "function")
          item.off('title-changed', () => this.updateTitle());
      }});
    }

    const pathChangedHandler = (path) => {
      this.path = path;
      this.updateDataAttributes();
      this.updateTitle();
      this.updateIcon();
    };

    if (typeof this.item.onDidChangePath === 'function') {
      const onDidChangePathDisposable = this.item.onDidChangePath(pathChangedHandler);
      if (Disposable.isDisposable(onDidChangePathDisposable)) {
        this.subscriptions.add(onDidChangePathDisposable);
      } else {
        console.warn("::onDidChangePath does not return a valid Disposable!", this.item);
      }
    } else if (typeof this.item.on === 'function') {
      //TODO Remove once old events are no longer supported
      this.item.on('path-changed', pathChangedHandler);
      this.subscriptions.add({ dispose: () => {
        if (typeof this.item.off === "function")
          this.item.off('path-changed', pathChangedHandler);
      }});
    }

    if (typeof this.item.onDidChangeIcon === 'function') {
      let onDidChangeIconDisposable;
      if (typeof this.item.onDidChangeIcon === "function") {
        onDidChangeIconDisposable = this.item.onDidChangeIcon(() => this.updateIcon());
      }
      if (Disposable.isDisposable(onDidChangeIconDisposable)) {
        this.subscriptions.add(onDidChangeIconDisposable);
      } else {
        console.warn("::onDidChangeIcon does not return a valid Disposable!", this.item);
      }
    } else if (typeof this.item.on === 'function') {
      //TODO Remove once old events are no longer supported
      this.item.on('icon-changed', () => this.updateIcon());
      this.subscriptions.add({ dispose: () => {
        if (typeof this.item.off === "function")
          this.item.off('icon-changed', () => this.updateIcon());
      }});
    }

    if (typeof this.item.onDidChangeModified === 'function') {
      const onDidChangeModifiedDisposable = this.item.onDidChangeModified(
        () => this.updateModifiedStatus()
      );
      if Disposable.isDisposable(onDidChangeModifiedDisposable) {
        this.subscriptions.add(onDidChangeModifiedDisposable);
      } else {
        console.warn("::onDidChangeModified does not return a valid Disposable!", this.item);
      }
    } else if (typeof this.item.on === 'function') {
      //TODO Remove once old events are no longer supported
      this.item.on('modified-status-changed', () => this.updateModifiedStatus())
      this.subscriptions.add({ dispose: () => {
        if (typeof this.item.off === "function")
          this.item.off('modified-status-changed', () => this.updateModifiedStatus());
      }
    }

    if (typeof this.item.onDidSave === 'function') {
      const onDidSaveDisposable = this.item.onDidSave((event) => {
        this.terminatePendingState();
        if (event.path !== this.path)
          this.path = event.path;
      });

      if (Disposable.isDisposable(onDidSaveDisposable)) {
        this.subscriptions.add(onDidSaveDisposable);
      } else {
        console.warn("::onDidSave does not return a valid Disposable!", this.item);
      }
    }
  }

  destroy() {
    this.subscriptions?.dispose();
    this.repoSubscriptions?.dispose();
    this.element.remove();
  }

  updateDataAttributes() {
    if (this.path) {
      this.itemTitle.dataset.name = path.basename(this.path);
      this.itemTitle.dataset.path = this.path;
    } else {
      delete this.itemTitle.dataset.name;
      delete this.itemTitle.dataset.path;
    }

    const itemClass = this.item.constructor?.name;
    if (itemClass) {
      this.element.dataset.type = itemClass;
    } else {
      delete this.element.dataset.type;
    }
  }

  updateTitle({updateSiblings, useLongTitle} = {}) {
    if (this.updatingTitle) { return; }
    this.updatingTitle = true;

    let title;
    let useLongTitle = useLongTitle;
    if (updateSiblings === false) {
      title = this.item.getTitle();
      if (useLongTitle) {
        let longTitle;
        if (typeof this.item.getLongTitle === "function")
          longTitle = this.item.getLongTitle();
        title = longTitle || title;
      }
      this.itemTitle.textContent = title;
    } else {
      title = this.item.getTitle();
      useLongTitle = false;
      for (const tab of this.tabs) {
        if (tab !== this && tab.item.getTitle() === title) {
          tab.updateTitle({ updateSiblings: false, useLongTitle: true });
          useLongTitle = true;
        }
      }
      if (useLongTitle) {
        let longTitle;
        if (typeof this.item.getLongTitle === "function")
          longTitle = this.item.getLongTitle();
        title = longTitle || title;
      }

      this.itemTitle.textContent = title;
    }

    this.updatingTitle = false;
  }

  updateIcon() {
    if (this.iconName) {
      const names = !Array.isArray(this.iconName)
        ? this.iconName.split(/\s+/g)
        : this.iconName;
      this.itemTitle.classList.remove('icon', `icon-${names[0]}`, ...names);
    }

    if (typeof this.item.getIconName === 'function') {
      this.iconName = this.item.getIconName();
    } else {
      this.iconName = null;
    }
    if (this.iconName) {
      this.itemTitle.classList.add('icon', `icon-${this.iconName}`);
    } else if (this.path != null) {
      this.iconName = iconClassForPath(this.path);
      if (this.iconName) {
        let names = this.iconName;
        if (!Array.isArray(names)) {
          names = names.toString().split(/\s+/g);
        }
        this.itemTitle.classList.add('icon', ...names);
      }
    }
  }

  isItemPending() {
    if (this.pane.getPendingItem != null) {
      return this.pane.getPendingItem() === this.item;
    } else if (this.item.isPending != null) {
      return this.item.isPending();
    }
  }

  terminatePendingState() {
    if (this.pane.clearPendingItem != null) {
      if (this.pane.getPendingItem() === this.item)
        this.pane.clearPendingItem();
    } else if (this.item.terminatePendingState != null) {
      this.item.terminatePendingState();
    }
  }

  clearPending() {
    this.itemTitle.classList.remove('temp');
    this.element.classList.remove('pending-tab');
  }

  updateModifiedStatus() {
    if (typeof this.item.isModified === "function" && base.isModified()) {
      if (!this.isModified)
        this.element.classList.add('modified');
      this.isModified = true;
    } else {
      if (this.isModified)
        this.element.classList.remove('modified');
      this.isModified = false;
    }
  }
}

class TabBarView {
  constructor(pane, location) {
    this.pane = pane;
    this.location = location;
    this.element = document.createElement('ul');
    this.element.classList.add("list-inline");
    this.element.classList.add("tab-bar");
    this.element.classList.add("inset-panel");
    this.element.setAttribute('is', 'atom-tabs');
    this.element.setAttribute("tabindex", -1);
    this.element.setAttribute("location", this.location);

    this.tabs = [];
    this.tabsByElement = new WeakMap();
    this.subscriptions = new CompositeDisposable();

    this.paneElement = this.pane.getElement();

    this.element.addEventListener('mouseenter', this.onMouseEnter.bind(this));
    this.element.addEventListener('mouseleave', this.onMouseLeave.bind(this));

    this.paneContainer = this.pane.getContainer();
    for (const item of this.pane.getItems()) {
      this.addTabForItem(item);
    }

    this.subscriptions.add(this.pane.onDidDestroy(
      () => this.destroy()
    ));

    this.subscriptions.add(this.pane.onDidAddItem(
      ({item, index}) => this.addTabForItem(item, index)
    ));

    this.subscriptions.add(this.pane.onDidMoveItem(
      ({item, newIndex}) => this.moveItemTabToIndex(item, newIndex)
    ));

    this.subscriptions.add(this.pane.onDidRemoveItem(
      ({item}) => this.removeTabForItem(item)
    ));

    this.subscriptions.add(this.pane.onDidChangeActiveItem(
      (item) => this.updateActiveTab()
    ));

    this.updateActiveTab();

    this.element.addEventListener("mousedown", this.onMouseDown.bind(this));
    this.element.addEventListener("click", this.onClick.bind(this));
    this.element.addEventListener("auxclick", this.onClick.bind(this));
  }

  destroy() {
    this.subscriptions.dispose();
    this.element.remove();
  }

  terminatePendingStates() {
    for (const tab of this.getTabs()) {
      if (typeof tab.terminatePendingState === "function")
        tab.terminatePendingState();
    }
  }

  addTabForItem(item, index) {
    const tabView = new TabView({
      item,
      pane: this.pane,
      tabs: this.tabs,
      didClickCloseIcon: () => {
        if (typeof tabView !== "undefined" && tabView !== null)
          this.pane.destroyItem(tabView.item);
      },
      location: this.location
    })
    if (this.isItemMovingBetweenPanes) tabView.terminatePendingState();
    this.tabsByElement.set(tabView.element, tabView);
    this.insertTabAtIndex(tabView, index);
  }

  moveItemTabToIndex(item, index) {
    const tabIndex = this.tabs.findIndex(t => t.item === item);
    if (tabIndex !== -1) {
      const tab = this.tabs[tabIndex];
      tab.element.remove();
      this.tabs.splice(tabIndex, 1);
      this.insertTabAtIndex(tab, index);
    }
  }

  insertTabAtIndex(tab, index) {
    let followingTab;
    if (typeof index !== "undefined" && index !== null)
      followingTab = this.tabs[index];
    if (followingTab) {
      this.element.insertBefore(tab.element, followingTab.element);
      this.tabs.splice(index, 0, tab);
    } else {
      this.element.appendChild(tab.element);
      this.tabs.push(tab);
    }

    tab.updateTitle();
  }

  removeTabForItem(item) {
    const tabIndex = this.tabs.findIndex(t => t.item === item)
    if (tabIndex !== -1) {
      const tab = this.tabs[tabIndex];
      this.tabs.splice(tabIndex, 1);
      this.tabsByElement.delete(tab);
      tab.destroy();
    }
    for (const tab of this.getTabs()) {
      tab.updateTitle();
    }
  }

  getTabs() {
    return this.tabs.slice();
  }

  tabAtIndex(index) {
    return this.tabs[index];
  }

  tabForItem(item) {
    return this.tabs.find(t => t.item === item);
  }

  setActiveTab(tabView) {
    if (
      (typeof tabView !== "undefined" && tabView !== null) &&
      tabView !== this.activeTab
    ) {
      this.activeTab?.element.classList.remove('active');
      this.activeTab = tabView;
      this.activeTab.element.classList.add('active');
      this.activeTab.element.scrollIntoView(false);
    }
  }

  getActiveTab() {
    return this.tabForItem(this.pane.getActiveItem());
  }

  updateActiveTab() {
    this.setActiveTab(this.tabForItem(this.pane.getActiveItem()));
  }

  onMouseDown(event) {
    const tab = this.tabForElement(event.target);
    if (!tab) { return; }

    if (event.which === 3 || (event.which === 1 && event.ctrlKey === true)) {
      event.preventDefault();
    } else if (event.which === 2) {
      // This prevents Chromium from activating "scroll mode" when
      // middle-clicking while some tabs are off-screen.
      event.preventDefault();
    }
  }

  onClick(event) {
    const tab = this.tabForElement(event.target);
    if (!tab) { return; }

    event.preventDefault();
    if (event.which === 3 || (event.which === 1 && event.ctrlKey === true)) {
      // Bail out early when receiving this event, because we have already
      // handled it in the mousedown handler.
      return;
    } else if (event.which === 1 && !event.target.classList.contains('close-icon')) {
      this.pane.activateItem(tab.item);
      if (!this.pane.isDestroyed()) this.pane.activate();
    } else if (event.which === 2) {
      this.pane.destroyItem(tab.item);
    }
  }

  onMouseEnter() {
    for (const tab of this.getTabs()) {
      {width} = tab.element.getBoundingClientRect();
      tab.element.style.maxWidth = width.toFixed(2) + 'px';
    }
  }

  onMouseLeave() {
    for (const tab of this.getTabs()) {
      tab.element.style.maxWidth = '';
    }
  }

  tabForElement(element) {
    let currentElement = element;
    while (typeof currentElement !== "undefined" && currentElement !== null) {
      const tab = this.tabsByElement.get(currentElement);
      if (tab) {
        return tab;
      } else {
        currentElement = currentElement.parentElement;
      }
    }
  }
}

module.exports.activate = function() {
  const paneContainers = {
    center: atom.workspace.getCenter(),
    left: atom.workspace.getLeftDock(),
    right: atom.workspace.getRightDock(),
    bottom: atom.workspace.getBottomDock()
  };

  for (const location in paneContainers) {
    const container = paneContainers[location];
    if (!container) { return; }

    container.observePanes(pane => {
      const tabBarView = new TabBarView(pane, location);

      const paneElement = pane.getElement();
      paneElement.insertBefore(tabBarView.element, paneElement.firstChild);
    })
  }
};
