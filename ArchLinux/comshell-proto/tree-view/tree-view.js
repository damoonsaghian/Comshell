const path = require('path');
const fs = require('fs-plus');
const _ = require('underscore-plus');
const Minimatch = require('minimatch').Minimatch;
const { Disposable, CompositeDisposable } = require('event-kit');
const { repoForPath, getStyleObject, getFullExtension } = require('./helpers');
const Directory = require('./directory');
const DirectoryView = require('./directory-view');
const { AddDialog, CopyDialog, MoveDialog } = require('./dialogs');
const Dialog = require('../dialog');

/*
.list-tree.has-collapsable-children .list-nested-item > .list-tree > li {
  padding-left: 17px;
}
*/
// https://github.com/claire-codes/rainbow-tree
// https://github.com/guippiccoli/pretty-dir
// https://atom.io/packages/tree-view-scope-lines
// https://atom.io/packages/tree-lines

// https://github.com/abe33/atom-tree-view-breadcrumb
// https://github.com/lixinliang/tree-view-search-bar
// https://github.com/subesokun/atom-tree-view-git-status
// https://github.com/ryanolsonx/atom-vim-mode-plus-keymaps-for-tree-view
// https://github.com/aviatesk/atom-tree-view-autoadjust

class IgnoredNames {
  constructor() {
    this.ignoredPatterns = [];

    let ignoredNames = atom.config.get('core.ignoredNames');
    if (ignoredNames == null)
      ignoredNames = [];
    if (typeof ignoredNames === 'string')
      ignoredNames = [ignoredNames];
    for (const ignoredName of ignoredNames) {
      if (ignoredName) {
        try {
          this.ignoredPatterns.push(
            new Minimatch(ignoredName, { matchBase: true, dot: true })
          );
        } catch (error) {
          atom.notifications.addWarning(
            `error parsing ignore pattern (${ignoredName})`,
            { detail: error.message }
          );
        }
      }
    }
  }

  matches(filePath) {
    for (ignoredPattern of this.ignoredPatterns) {
      if (ignoredPattern.match(filePath))
        return true;
    }

    return false;
  }
}

class TreeView {
  constructor(state) {
    this.element = document.createElement('div');
    this.element.classList.add('tool-panel', 'tree-view');
    this.element.tabIndex = -1;

    this.list = document.createElement('ol');
    this.list.classList.add(
      'tree-view-root', 'full-menu', 'list-tree', 'has-collapsable-children', 'focusable-panel'
    );

    this.disposables = new CompositeDisposable();
    this.emitter = new Emitter();
    this.roots = [];
    this.selectedPath = null;
    this.lastFocusedEntry = null;
    this.ignoredPatterns = [];
    this.useSyncFS = false;
    this.currentlyOpening = new Map();
    this.editorsToMove = [];
    this.editorsToDestroy = [];

    this.handleEvents();

    process.nextTick(() => {
      this.onStylesheetsChanged();
      onStylesheetsChanged = _.debounce(this.onStylesheetsChanged, 100);
      this.disposables.add(atom.styles.onDidAddStyleElement(onStylesheetsChanged));
      this.disposables.add(atom.styles.onDidRemoveStyleElement(onStylesheetsChanged));
      this.disposables.add(atom.styles.onDidUpdateStyleElement(onStylesheetsChanged));
    });

    this.updateRoots(state.directoryExpansionStates);

    if (state.selectedPaths?.length > 0) {
      for (const selectedPath of state.selectedPaths) {
        this.selectMultipleEntries(this.entryForPath(selectedPath));
      }
    } else {
      this.selectEntry(this.roots[0]);
    }

    if (state.scrollTop != null || state.scrollLeft != null) {
      const observer = new IntersectionObserver(() => {
        if (this.isVisible()) {
          this.element.scrollTop = state.scrollTop;
          this.element.scrollLeft = state.scrollLeft;
          return observer.disconnect();
        }
      });
      observer.observe(this.element);
    }

    if (state.width > 0)
      this.element.style.width = `${state.width}px`;

    this.disposables.add(this.onWillMoveEntry(({ initialPath, newPath }) => {
      const editors = atom.workspace.getTextEditors();
      let initialPath = initialPath;
      if (fs.isDirectorySync(initialPath)) {
        initialPath += path.sep; // Avoid moving lib2's editors when lib was moved
        for (const editor of editors) {
          const filePath = editor.getPath();
          if (filePath?.startsWith(initialPath))
            this.editorsToMove.push(filePath);
        }
      } else {
        for (const editor of editors) {
          const filePath = editor.getPath();
          if (filePath === initialPath)
            this.editorsToMove.push(filePath);
        }
      }
    }));

    this.disposables.add(this.onEntryMoved(({ initialPath, newPath }) => {
      for (const editor of atom.workspace.getTextEditors()) {
        const filePath = editor.getPath();
        const index = this.editorsToMove.indexOf(filePath);
        if (index !== -1) {
          editor.getBuffer().setPath(filePath.replace(initialPath, newPath));
          this.editorsToMove.splice(index, 1);
        }
      }
    }));

    this.disposables.add(this.onMoveEntryFailed(({ initialPath, newPath }) => {
      const index = this.editorsToMove.indexOf(initialPath);
      if (index !== -1) this.editorsToMove.splice(index, 1);
    }));

    this.disposables.add(this.onWillDeleteEntry(({ pathToDelete }) => {
      const editors = atom.workspace.getTextEditors();
      let pathToDelete = pathToDelete;
      if (fs.isDirectorySync(pathToDelete)) {
        pathToDelete += path.sep; // Avoid destroying lib2's editors when lib was deleted
        for (const editor of editors) {
          const filePath = editor.getPath();
          if (filePath?.startsWith(pathToDelete) && !editor.isModified())
            this.editorsToDestroy.push(filePath);
        }
      } else {
        for (const editor of editors) {
          const filePath = editor.getPath();
          if (filePath === pathToDelete && !editor.isModified())
            this.editorsToDestroy.push(filePath);
        }
      }
    }));

    this.disposables.add(this.onEntryDeleted(({ pathToDelete }) => {
      for (const editor of atom.workspace.getTextEditors()) {
        const index = this.editorsToDestroy.indexOf(editor.getPath());
        if (index !== -1) {
          editor.destroy();
          this.editorsToDestroy.splice(index, 1);
        }
      }
    }));
  }

  serialize() {
    return {
      directoryExpansionStates: new function(roots) {
        for (const root of roots) {
          this[root.directory.path] = root.directory.serializeExpansionState();
        }
        return this;
      }(this.roots),
      deserializer: 'TreeView',
      selectedPaths: Array.from(
        this.getSelectedEntries(),
        (entry) => entry.getPath()
      ),
      scrollLeft: this.element.scrollLeft,
      scrollTop: this.element.scrollTop,
      width: parseInt(this.element.style.width || 0)
    }
  }

  destroy() {
    for (const root of this.roots) {
      root.directory.destroy();
    }
    this.disposables.dispose();
    this.emitter.emit('did-destroy');
  }

  onDidDestroy(callback) {
    this.emitter.on('did-destroy', callback);
  }

  getTitle() { return 'project'; }

  getURI() { return 'atom://tree-view'; }

  getPreferredLocation() {
    if (atom.config.get('tree-view.showOnRightSide')) {
      return 'right';
    } else {
      return 'left';
    }
  }

  getAllowedLocations() { return ["left", "right"]; }

  isPermanentDockItem() { return true; }

  getPreferredWidth() {
    this.list.style.width = 'min-content';
    const result = this.list.offsetWidth;
    this.list.style.width = '';
    return result;
  }

  onDirectoryCreated(callback) {
    this.emitter.on('directory-created', callback);
  }

  onEntryCopied(callback) {
    this.emitter.on('entry-copied', callback);
  }

  onWillDeleteEntry(callback) {
    this.emitter.on('will-delete-entry', callback);
  }

  onEntryDeleted(callback) {
    this.emitter.on('entry-deleted', callback);
  }

  onWillMoveEntry(callback) {
    this.emitter.on('will-move-entry', callback);
  }

  onEntryMoved(callback) {
    this.emitter.on('entry-moved', callback);
  }

  onMoveEntryFailed(callback) {
    this.emitter.on('move-entry-failed', callback);
  }

  onFileCreated(callback) {
    this.emitter.on('file-created', callback);
  }

  handleEvents() {
    this.element.addEventListener('click', (e) => {
      // this prevents accidental collapsing when a .entries element is the event target;
      if (e.target.classList.contains('entries')) return;
      if (!e.shiftKey && !e.metaKey && !e.ctrlKey) this.entryClicked(e);
    });

    atom.commands.add(this.element, {
      'core:move-up': (e) => this.moveUp(e),
      'core:move-down': (e) => this.moveDown(e),
      'core:page-up': () => this.pageUp(),
      'core:page-down': () => this.pageDown(),
      'core:move-to-top': () => this.scrollToTop(),
      'core:move-to-bottom': () => this.scrollToBottom(),
      'tree-view:expand-item': () => this.openSelectedEntry(pending: true, true),
      'tree-view:collapse-directory': () => this.collapseDirectory(),
      'tree-view:open-selected-entry': () => this.openSelectedEntry(),
      'tree-view:move': () => this.moveSelectedEntry(),
      'tree-view:copy': () => this.copySelectedEntries(),
      'tree-view:cut': () => this.cutSelectedEntries(),
      'tree-view:paste': () => this.pasteEntries(),
      'tree-view:copy-full-path': () => this.copySelectedEntryPath(false),
      'tree-view:copy-project-path': () => this.copySelectedEntryPath(true),
      'tree-view:unfocus': () => this.unfocus()
    });

    this.disposables.add(atom.workspace.getCenter().onDidChangeActivePaneItem(
      () => this.selectActiveFile()
    ));
    this.disposables.add(
      atom.project.onDidChangePaths(() => this.updateRoots())
    );
    this.disposables.add(atom.config.onDidChange(
      'core.ignoredNames',
      () => this.updateRoots()
    ));
  }

  toggle() {
    atom.workspace.toggle(this);
  }

  show(focus) {
    atom.workspace.open(this, {
      searchAllPanes: true,
      activatePane: false,
      activateItem: false,
    }).then(() => {
      atom.workspace.paneContainerForURI(this.getURI()).show();
      if (focus) this.focus();
    });
  }

  hide() {
    atom.workspace.hide(this);
  }

  focus() {
    this.element.focus();
  }

  unfocus() {
    atom.workspace.getCenter().activate();
  }

  hasFocus() {
    return document.activeElement === this.element;
  }

  toggleFocus() {
    if (this.hasFocus()) {
      this.unfocus();
    } else {
      this.show(true);
    }
  }

  entryClicked(e) {
    const entry = e.target.closest('.entry');
    if (entry) {
      const isRecursive = e.altKey || false;
      this.selectEntry(entry);
      if (entry.classList.contains('directory')) {
        entry.toggleExpansion(isRecursive);
      } else if (entry.classList.contains('file')) {
        this.fileViewEntryClicked(e);
      }
    }
  }

  fileViewEntryClicked(e) {
    const filePath = e.target.closest('.entry').getPath();
    let detail = e.detail;
    if (detail == null) detail = 1;

    if (detail === 1) {
      if (atom.config.get('core.allowPendingPaneItems')) {
        const openPromise = atom.workspace.open(filePath, {
          pending: true,
          activatePane: false
        });
        this.currentlyOpening.set(filePath, openPromise);
        openPromise.then(() => this.currentlyOpening.delete(filePath));
      }
    } else if (detail === 2) {
      this.openAfterPromise(filePath)
    }
  }

  openAfterPromise(uri, options) {
    const promise = this.currentlyOpening.get(uri);
    if (promise) {
      promise.then(() => atom.workspace.open(uri, options));
    } else {
      atom.workspace.open(uri, options);
    }
  }

  updateRoots(expansionStates = {}) {
    const selectedPaths = this.selectedPaths();

    const oldExpansionStates = {};
    for (const root of this.roots) {
      oldExpansionStates[root.directory.path] = root.directory.serializeExpansionState();
      root.directory.destroy();
      root.remove();
    }

    this.roots = [];

    projectPaths = atom.project.getPaths();
    if (projectPaths.length > 0) {
      if (!this.element.querySelector('tree-view-root'))
        this.element.appendChild(this.list);

      for (const projectPath of projectPaths) {
        let stats = fs.lstatSyncNoException(projectPath);
        if (!stats) continue;
        stats = _.pick(stats, ..._.keys(stats));
        for (const key of ["atime", "birthtime", "ctime", "mtime"]) {
          stats[key] = stats[key].getTime();
        }

        const directory = new Directory({
          name: path.basename(projectPath),
          fullPath: projectPath,
          symlink: false,
          isRoot: true,
          expansionState:
            expansionStates[projectPath] ??
            oldExpansionStates[projectPath] ??
            {isExpanded: true},
          ignoredNames: new IgnoredNames(),
          useSyncFS: this.useSyncFS,
          stats
        });

        const root = new DirectoryView(directory).element;
        this.list.appendChild(root);
        this.roots.push(root);
      }

      // The DOM has been recreated; reselect everything
      for (const selectedPath of selectedPaths) {
        this.selectMultipleEntries(this.entryForPath(selectedPath));
      }
    } else {
      if (this.element.querySelector('.tree-view-root'))
        this.element.removeChild(this.list);
    }
  }

  getActivePath() {
    const getPath = atom.workspace.getCenter().getActivePaneItem()?.getPath;
    if (typeof getPath === "function")
      return getPath();
  }

  selectActiveFile() {
    const activeFilePath = this.getActivePath();
    if (this.entryForPath(activeFilePath)) {
      this.selectEntryForPath(activeFilePath);
    } else {
      // if the active file is not part of the project, deselect all entries;
      this.deselect();
    }
  }

  revealActiveFile(options = {}) {
    if (!atom.project.getPaths().length) return Promise.resolve();

    const {show, focus} = options;

    promise = (show || focus) ? this.show(focus) : Promise.resolve();
    promise.then(() => {
      const activeFilePath = this.getActivePath();
      if (!activeFilePath) return;

      [rootPath, relativePath] = atom.project.relativizePath(activeFilePath);
      if (typeof rootPath === "undefined" || rootPath === null) return;

      const activePathComponents = relativePath.split(path.sep);
      // Add the root folder to the path components
      activePathComponents.unshift(rootPath.substr(rootPath.lastIndexOf(path.sep) + 1));
      // And remove it from the current path
      let currentPath = rootPath.substr(0, rootPath.lastIndexOf(path.sep));
      for (const pathComponent of activePathComponents) {
        currentPath += path.sep + pathComponent;
        const entry = this.entryForPath(currentPath);
        if (entry.classList.contains('directory')) {
          entry.expand();
        } else {
          this.selectEntry(entry);
          this.scrollToEntry(entry);
        }
      }
    });
  }

  copySelectedEntryPath(relativePath = false) {
    let pathToCopy = this.selectedPath;
    if (pathToCopy) {
      if (relativePath) pathToCopy = atom.project.relativize(pathToCopy) ;
      atom.clipboard.write(pathToCopy);
  }

  entryForPath(entryPath) {
    let bestMatchEntry = null;
    let bestMatchLength = 0;

    for (const entry of this.list.querySelectorAll('.entry')) {
      if (entry.isPathEqual(entryPath))
        return entry;

      const entryLength = entry.getPath().length;
      if (entry.directory?.contains(entryPath) && entryLength > bestMatchLength) {
        bestMatchEntry = entry;
        bestMatchLength = entryLength;
      }
    }

    return bestMatchEntry;
  }

  selectEntryForPath(entryPath) {
    return this.selectEntry(this.entryForPath(entryPath));
  }

  moveDown(event) {
    event?.stopImmediatePropagation();
    const selectedEntry = this.selectedEntry();
    if (typeof selectedEntry !== "undefined" && selectedEntry !== null) {
      if (selectedEntry.classList.contains('directory')) {
        if (this.selectEntry(selectedEntry.entries.children[0])) {
          this.scrollToEntry(this.selectedEntry(), false);
          return;
        }
      }

      const nextEntry = this.nextEntry(selectedEntry);
      if (nextEntry) this.selectEntry(nextEntry);
    } else {
      this.selectEntry(this.roots[0]);
    }

    this.scrollToEntry(this.selectedEntry(), false);
  }

  moveUp(event) {
    event.stopImmediatePropagation();
    const selectedEntry = this.selectedEntry();
    if (typeof selectedEntry !== "undefined" && selectedEntry !== null) {
      const previousEntry = this.previousEntry(selectedEntry);
      if (previousEntry) {
        this.selectEntry(previousEntry);
      } else {
        this.selectEntry(selectedEntry.parentElement.closest('.directory'));
      }
    } else {
      const entries = this.list.querySelectorAll('.entry');
      this.selectEntry(entries[entries.length - 1]);
    }

    this.scrollToEntry(this.selectedEntry(), false);
  }

  nextEntry(entry) {
    let currentEntry = entry;
    while (typeof currentEntry !== "undefined" && currentEntry !== null) {
      if (currentEntry.nextSibling != null) {
        currentEntry = currentEntry.nextSibling;
        if (currentEntry.matches('.entry'))
          return currentEntry;
      } else {
        currentEntry = currentEntry.parentElement.closest('.directory');
      }
    }

    return null;
  }

  previousEntry(entry) {
    let previousEntry = entry.previousSibling;
    while (
      typeof previousEntry !== "undefined" &&
      previousEntry !== null &&
      !previousEntry.matches('.entry')
    ) {
      previousEntry = previousEntry.previousSibling;
    }

    if (typeof previousEntry === "undefined" || previousEntry === null)
      return null;

    // If the previous entry is an expanded directory,
    // we need to select the last entry in that directory,
    // not the directory itself
    if (previousEntry.matches('.directory.expanded')) {
      const entries = previousEntry.querySelectorAll('.entry');
      if (entries.length > 0)
        return entries[entries.length - 1];
    }

    return previousEntry;
  }

  expandDirectory(isRecursive=false) {
    const selectedEntry = this.selectedEntry();
    if (typeof selectedEntry === "undefined" || selectedEntry === null)
      return;

    const directory = selectedEntry.closest('.directory');
    if (isRecursive === false && directory.isExpanded) {
      // Select the first entry in the expanded folder if it exists
      if (directory.directory.getEntries().length > 0)
        this.moveDown();
    } else {
      directory.expand(isRecursive);
    }
  }

  collapseDirectory(isRecursive = false, allDirectories = false) {
    if (allDirectories) {
      for (const root of this.roots) {
        root.collapse(true);
      }
      return;
    }

    const selectedEntry = this.selectedEntry();
    if (typeof selectedEntry === "undefined" || selectedEntry === null)
      return;

    const directory = selectedEntry.closest('.expanded.directory');
    if (directory = selectedEntry.closest('.expanded.directory')) {
      directory.collapse(isRecursive);
      this.selectEntry(directory);
    }
  }

  openSelectedEntry(options = {}, expandDirectory = false) {
    const selectedEntry = this.selectedEntry();
    if (typeof selectedEntry === "undefined" || selectedEntry === null)
      return;

    if (selectedEntry.classList.contains('directory')) {
      if (expandDirectory) {
        this.expandDirectory(false);
      } else {
        selectedEntry.toggleExpansion();
      }
    } else if (selectedEntry.classList.contains('file')) {
      this.openAfterPromise(selectedEntry.getPath(), options);
    }
  }

  moveSelectedEntry() {
    let entry, oldPath;
    if (this.hasFocus()) {
      entry = this.selectedEntry();
      if (
        (typeof entry === "undefined" || entry === null) ||
        this.roots.includes(entry);
      ) return;
      oldPath = entry.getPath();
    } else {
      oldPath = this.getActivePath();
    }

    if (oldPath) {
      const dialog = new MoveDialog(oldPath, {
        willMove: ({initialPath, newPath}) =>
          this.emitter.emit('will-move-entry', {initialPath, newPath}),
        onMove: ({initialPath, newPath}) =>
          this.emitter.emit('entry-moved', {initialPath, newPath}),
        onMoveFailed: ({initialPath, newPath}) =>
          this.emitter.emit('move-entry-failed', {initialPath, newPath})
      });
      dialog.attach();
    }
  }

  copySelectedEntry() {
    let entry, oldPath;
    if (this.hasFocus()) {
      entry = this.selectedEntry()
      if (this.roots.includes(entry)) return;
      oldPath = entry?.getPath();
    } else {
      oldPath = this.getActivePath();
    }
    if (!oldPath) return;

    dialog = new CopyDialog(oldPath, {
      onCopy: ({initialPath, newPath}) =>
        this.emitter.emit('entry-copied', {initialPath, newPath})
    });
    dialog.attach();
  }

  removeSelectedEntries() {
    const activePath = this.getActivePath();
    let selectedPaths, selectedEntries;
    if (this.hasFocus()) {
      selectedPaths = this.selectedPaths();
      selectedEntries = this.getSelectedEntries();
    } else if (activePath) {
      selectedPaths = [activePath];
      selectedEntries = [this.entryForPath(activePath)];
    }

    if (!(selectedPaths?.length > 0)) return;

    for (const root in this.roots) {
      if (selectedPaths.includes(root.getPath())) {
        atom.notifications.addWarning("The root directory can't be removed.");
        return;
      }
    }

    const message = `delete these files?\n${selectedPaths.join('\n')}`;
    const saveDialog = new Dialog({ prompt: message });
    saveDialog.onConfirm = (answer) => {
      if (answer === 'y' || answer === 'yes') {
        saveDialog.close();

        for (const selectedPath of selectedPaths) {
          // Don't delete entries which no longer exist. This can happen, for example, when:
          // * The entry is deleted outside of Atom before "Move to Trash" is selected
          // * A folder and one of its children are both selected for deletion,
          //   but the parent folder is deleted first
          if (!fs.existsSync(selectedPath)) continue;

          this.emitter.emit('will-delete-entry', { pathToDelete: selectedPath });
          fs.removeSync(selectedPath);
          this.emitter.emit('entry-deleted', { pathToDelete: selectedPath })
          const repo = repoForPath(selectedPath);
          if (repo) repo.getPathStatus(selectedPath);
        }

        // Focus the first parent folder
        const firstSelectedEntry = selectedEntries[0];
        if (firstSelectedEntry)
          this.selectEntry(firstSelectedEntry.closest('.directory:not(.selected)'));
      } else {
        saveDialog.showError('enter "y" or "yes", or press "escape" to cancle');
      }
    };
    saveDialog.attach();
  }

  // Public: Copy the path of the selected entry element.
  //         Save the path in localStorage, so that copying from 2 different
  //         instances of atom works as intended
  //
  //
  // Returns `copyPath`.
  copySelectedEntries() {
    const selectedPaths = this.selectedPaths();
    if (!selectedPaths || selectedPaths.length <= 0) return;
    // save to localStorage so we can paste across multiple open apps
    window.localStorage.removeItem('tree-view:cutPath');
    window.localStorage['tree-view:copyPath'] = JSON.stringify(selectedPaths);
  }

  // Public: Cut the path of the selected entry element.
  //         Save the path in localStorage, so that cutting from 2 different
  //         instances of atom works as intended
  //
  //
  // Returns `cutPath`
  cutSelectedEntries() {
    const selectedPaths = this.selectedPaths();
    if (!selectedPaths || selectedPaths.length <= 0) return;
    // save to localStorage so we can paste across multiple open apps
    window.localStorage.removeItem('tree-view:copyPath');
    window.localStorage['tree-view:cutPath'] = JSON.stringify(selectedPaths);
  }

  // Public: Paste a copied or cut item.
  //         If a file is selected, the file's parent directory is used as the
  //         paste destination.
  async pasteEntries() {
    const selectedEntry = this.selectedEntry();
    if (!selectedEntry) return;

    const cutPaths = window.localStorage['tree-view:cutPath'] ?
      JSON.parse(window.localStorage['tree-view:cutPath']) : null;
    const copiedPaths = window.localStorage['tree-view:copyPath'] ?
      JSON.parse(window.localStorage['tree-view:copyPath']) : null;
    const initialPaths = copiedPaths || cutPaths;
    if (!initialPaths?.length) return;

    let newDirectoryPath = selectedEntry.getPath();
    if (selectedEntry.classList.contains('file'))
      newDirectoryPath = path.dirname(newDirectoryPath);

    for (const initialPath of initialPaths) {
      if (fs.existsSync(initialPath)) {
        if (copiedPaths) {
          this.copyEntry(initialPath, newDirectoryPath);
        } else if (cutPaths) {
          if (! await this.moveEntry(initialPath, newDirectoryPath)) break;
        }
      }
    }
  }

  add(isCreatingFile) {
    const selectedEntry = this.selectedEntry() ?? this.roots[0];
    const selectedPath = selectedEntry?.getPath() ?? '';

    const dialog = new AddDialog(selectedPath, isCreatingFile)
    dialog.onDidCreateDirectory((createdPath) => {
      this.entryForPath(createdPath)?.reload();
      this.selectEntryForPath(createdPath);
      this.emitter.emit('directory-created', { path: createdPath });
    });
    dialog.onDidCreateFile((createdPath) => {
      this.entryForPath(createdPath)?.reload();
      atom.workspace.open(createdPath);
      this.emitter.emit('file-created', { path: createdPath });
    });
    dialog.attach();
  }

  selectedEntry() {
    return this.list.querySelector('.selected');
  }

  selectEntry(entry) {
    if (entry == null) return;

    this.selectedPath = entry.getPath();
    this.lastFocusedEntry = entry;

    const selectedEntries = this.getSelectedEntries();
    if (selectedEntries.length > 1 || selectedEntries[0] !== entry) {
      this.deselect(selectedEntries);
      entry.classList.add('selected');
    }
    return entry;
  }

  getSelectedEntries() {
    return this.list.querySelectorAll('.selected');
  }

  deselect(elementsToDeselect) {
    const elementsToDeselect = elementsToDeselect ?? this.getSelectedEntries();
    for (const selected of elementsToDeselect) {
      selected.classList.remove('selected');
    }
  }

  scrollTop(top) {
    if (top != null) {
      return this.element.scrollTop = top;
    } else {
      return this.element.scrollTop;
    }
  }

  scrollBottom(bottom) {
    if (bottom != null) {
      return this.element.scrollTop = bottom - this.element.offsetHeight;
    } else {
      return this.element.scrollTop + this.element.offsetHeight;
    }
  }

  scrollToEntry(entry, center = true) {
    const element = entry?.classList.contains('directory') ? entry.header : entry;
    element?.scrollIntoViewIfNeeded(center);
  }

  scrollToBottom() {
    const lastEntry = _.last(this.list.querySelectorAll('.entry'));
    if (lastEntry) {
      this.selectEntry(lastEntry);
      this.scrollToEntry(lastEntry);
    }
  }

  scrollToTop() {
    if (this.roots[0] != null)
      this.selectEntry(this.roots[0]);
    this.element.scrollTop = 0;
  }

  pageUp() {
    this.element.scrollTop -= this.element.offsetHeight;
  }

  pageDown() {
    this.element.scrollTop += this.element.offsetHeight;
  }

  // Copies an entry from `initialPath` to `newDirectoryPath`
  // If the entry already exists in `newDirectoryPath`, a number is appended to the basename
  copyEntry(initialPath, newDirectoryPath) {
    const initialPathIsDirectory = fs.isDirectorySync(initialPath);

    // Do not allow copying test/a/ into test/a/b/
    // Note: A trailing path.sep is added to prevent false positives, such as test/a -> test/ab
    const realNewDirectoryPath = fs.realpathSync(newDirectoryPath) + path.sep;
    const realInitialPath = fs.realpathSync(initialPath) + path.sep;
    if (initialPathIsDirectory && realNewDirectoryPath.startsWith(realInitialPath)) {
      if (!fs.isSymbolicLinkSync(initialPath)) {
        atom.notifications.addWarning('Cannot copy a folder into itself');
        return;
      }
    }

    let newPath = path.join(newDirectoryPath, path.basename(initialPath));

    // append a number to the file if an item with the same name exists
    let fileCounter = 0;
    const originalNewPath = newPath;
    while (fs.existsSync(newPath)) {
      if (initialPathIsDirectory) {
        newPath = `${originalNewPath}${fileCounter}`;
      } else {
        extension = getFullExtension(originalNewPath);
        filePath = path.join(
          path.dirname(originalNewPath),
          path.basename(originalNewPath, extension)
        );
        newPath = `${filePath}${fileCounter}${extension}`;
      }
      fileCounter += 1;
    }

    try {
      this.emitter.emit('will-copy-entry', {initialPath, newPath});
      if (initialPathIsDirectory) {
        // use fs.copy to copy directories since read/write will fail for directories
        fs.copySync(initialPath, newPath);
      } else {
        // read the old file and write a new one at target location
        // TODO: Replace with fs.copyFileSync
        fs.writeFileSync(newPath, fs.readFileSync(initialPath));
      }
      this.emitter.emit('entry-copied', {initialPath, newPath});

      const repo = repoForPath(newPath);
      if (repo) {
        repo.getPathStatus(initialPath);
        repo.getPathStatus(newPath);
      }
    } catch (error) {
      this.emitter.emit('copy-entry-failed', {initialPath, newPath});
      atom.notifications.addWarning(
        `Failed to copy entry ${initialPath} to ${newDirectoryPath}`,
        { detail: error.message }
      );
    }
  }

  // Moves an entry from `initialPath` to `newDirectoryPath`
  async moveEntry(initialPath, newDirectoryPath) {
    // Do not allow moving test/a/ into test/a/b/
    // Note: A trailing path.sep is added to prevent false positives, such as test/a -> test/ab
    const realNewDirectoryPath = fs.realpathSync(newDirectoryPath) + path.sep;
    const realInitialPath = fs.realpathSync(initialPath) + path.sep;
    if (fs.isDirectorySync(initialPath) && realNewDirectoryPath.startsWith(realInitialPath)) {
      if (!fs.isSymbolicLinkSync(initialPath)) {
        atom.notifications.addWarning('Cannot move a folder into itself');
        return;
      }
    }

    const newPath = path.join(newDirectoryPath, path.basename(initialPath));

    try {
      this.emitter.emit('will-move-entry', {initialPath, newPath});
      fs.moveSync(initialPath, newPath);
      this.emitter.emit('entry-moved', {initialPath, newPath});

      const repo = repoForPath(newPath);
      if (repo) {
        repo.getPathStatus(initialPath);
        repo.getPathStatus(newPath);
      }

    } catch (error) {
      if (error.code === 'EEXIST') {
        return await this.moveConflictingEntry(initialPath, newPath, newDirectoryPath);
      } else {
        this.emitter.emit('move-entry-failed', {initialPath, newPath});
        atom.notifications.addWarning(
          `Failed to move entry ${initialPath} to ${newDirectoryPath}`,
          { detail: error.message }
        );
      }
    }

    return true;
  }

  async moveConflictingEntry(initialPath, newPath, newDirectoryPath) {
    try {
      if (!fs.isDirectorySync(initialPath)) {
        // Files, symlinks, anything but a directory
        const dialogPromise = new Promise((resolve, reject) => {
          const message =
            `'${path.relative(newDirectoryPath, newPath)}' already exists; replace or skip?`;
          const saveDialog =
            new Dialog({ prompt: message , defaultAnswer: 'replace', select: true });

          saveDialog.onCancel = () => resolve('cancel');
          saveDialog.onConfirm = (answer) => {
            if (answer === 'r' || answer === 'replace') {
              saveDialog.close();
              resolve('replace');
            } else if (answer === 's' || answer === 'skip') {
              saveDialog.close();
              resolve('skip');
            } else {
              saveDialog.showError('enter "r" to replace, or "s" to skip');
            }
          };
          saveDialog.attach();
        });

        switch (await dialogPromise) {
          case 'replace':
            fs.renameSync(initialPath, newPath);
            this.emitter.emit('entry-moved', {initialPath, newPath});

            const repo = repoForPath(newPath);
            if (repo) {
              repo.getPathStatus(initialPath);
              repo.getPathStatus(newPath);
            }
            break;
          case 'cancel':
            return false
        }

      } else {
        const entries = fs.readdirSync(initialPath);
        for (const entry of entries) {
          if (fs.existsSync(path.join(newPath, entry))) {
            const result = await this.moveConflictingEntry(
              path.join(initialPath, entry),
              path.join(newPath, entry),
              newDirectoryPath
            );
            if (!result) return false;
          } else {
            await this.moveEntry(path.join(initialPath, entry), newPath);
          }
        }

        // "Move" the containing folder by deleting it, since we've already moved everything in it
        if (!fs.readdirSync(initialPath).length) fs.rmdirSync(initialPath);
      }
    } catch (error) {
      this.emitter.emit('move-entry-failed', {initialPath, newPath});
      atom.notifications.addWarning(
        `Failed to move entry ${initialPath} to ${newPath}`,
        { detail: error.message }
      );
    }

    return true;
  }

  onStylesheetsChanged() {
    // If visible, force a redraw so the scrollbars are styled correctly based on
    // the theme
    if (!this.isVisible()) return;
    this.element.style.display = 'none';
    this.element.offsetWidth;
    this.element.style.display = '';
  }

  // Public: Return an array of paths from all selected items
  //
  // Example: @selectedPaths()
  // => ['selected/path/one', 'selected/path/two', 'selected/path/three']
  // Returns Array of selected item paths
  selectedPaths() {
    let results = [];
    for (const entry of this.getSelectedEntries()) {
      results.push(entry.getPath());
    }
    return result;
  }

  // Public: Selects items within a range defined by a currently selected entry and
  //         a new given entry. This is shift+click functionality
  //
  // Returns array of selected elements
  selectContinuousEntries(entry, deselectOthers = true) {
    const currentSelectedEntry = this.lastFocusedEntry ?? this.selectedEntry();
    const parentContainer = entry.parentElement;

    let elements = [];
    if (parentContainer === currentSelectedEntry.parentElement) {
      const entries = Array.from(parentContainer.querySelectorAll('.entry'));
      const entryIndex = entries.indexOf(entry);
      const selectedIndex = entries.indexOf(currentSelectedEntry)
      for (const i of _.range(entryIndex, selectedIndex+1)) {
        elements.push(entries[i]);
      }

      if (deselectOthers) this.deselect();
      for (const element of elements) {
        element.classList.add('selected');
      }
    }
    return elements;
  }

  // Public: Selects consecutive given entries without clearing previously selected
  //         items. This is cmd+click functionality
  //
  // Returns given entry
  selectMultipleEntries(entry) {
    entry?.classList.toggle('selected');
    return entry;
  }

  isVisible() {
    return this.element.offsetWidth !== 0 || this.element.offsetHeight !== 0;
  }
}

module.exports.activate = function() {
  const treeView = new TreeView(state);
  const showOnAttach = !atom.workspace.getActivePaneItem();
  this.treeViewOpenPromise = atom.workspace.open(treeView, {
    activatePane: showOnAttach,
    activateItem: showOnAttach
  });

  atom.commands.add('atom-workspace', {
    'tree-view:add-file': () => treeView.add(true),
    'tree-view:add-folder': () => treeView.add(false),
    'tree-view:duplicate': () => treeView.copySelectedEntry(),
    'tree-view:remove': () => treeView.removeSelectedEntries(),
    'tree-view:rename': () => treeView.moveSelectedEntry()
  });

  // remove disclosure arrows in tree_view;
  treeView.list.classList.remove('has-collapsable-children');

  // numerical sorting: 10 after 9;
  //treeView.roots[0].directory.constructor.prototype.sortEntries = sortEntries;

  treeView.roots.forEach(root => {
    root.collapse(true);
    root.expand(false);
  });
  treeView.revealActiveFile({ show: false, focus: false });

  atom.workspace.onDidStopChangingActivePaneItem(() => {
    if (atom.workspace.getActivePane() === atom.workspace.getCenter().getActivePane()) {
      treeView.roots.forEach(root => {
        root.collapse(true);
        root.expand(false);
      });
      treeView.revealActiveFile({ show: false, focus: false });
    }
  });
}

function sortEntries(combinedEntries) {
  // https://github.com/hex-ci/atom-tree-view-sort/blob/master/lib/tree-view-sort.js#L158
  return combinedEntries.sort((first, second) => {
    const firstName = first.name ? first.name.toLowerCase() : first;
    const secondName = second.name ? second.name.toLowerCase() : second;
    return firstName.localeCompare(secondName)
  });
}
