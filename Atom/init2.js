// one pane per project
// only the active project's pane is displayed, others will be hidden;

atom.enablePersistence = false;

const fs = require('fs');
const path = require('path');
const SelectList = global.require('atom-select-list');

const projectsDir = path.join(require('os').homedir(), 'projects');
// if "~/projects/" directory does not exist, create it;
fs.stat(projectsDir, (err, stats) => {
  if (err) {
    fs.mkdir(projectsDir, (err) => { if (err) alert(err.message); });
  }
  else if (!stats.isDirectory()) {
    alert('can\'t create projects directory, cause there is a file with the same name');
  }
});

// { 'project name': projectPane }
const projectPanes = {};

// state: editors, buffers
// state is stored/restored to/from these two files in the ".cache" directory of each project:
//   "state/editors" and "state/buffers";

function storeBuffers(projectName) {
  const projectPane = projectPanes[projectName];
  if (!projectPane) return;

  const data = projectPane.getItems().filter(item => atom.workspace.isTextEditor(item))
    .map(editor =>
      JSON.stringify(editor.getBuffer().serialize())
    );
  const storePath = path.join(projectsDir, projectName, '.cache/state/buffers');
  fs.writeFile(storePath, data, (err) => {
    if (err) fs.mkdir(path.dirname(storePath), { recursive: true }, (err) => {
      if (err) { console.error(err) } else {
        fs.writeFile(storePath, data, (err) => { if (err) console.error(err); })
      }
    });
  });
}

const projectsWithChangedBuffers = new Set();
setInterval(() => {
  projectsWithChangedBuffers.forEach(projectName => storeBuffers(projectName));
  projectsWithChangedBuffers.clear();
}, 15000);
atom.window.addEventListener('beforeunload', (_) => {
  projectsWithChangedBuffers.forEach(projectName => storeBuffers(projectName));
  projectsWithChangedBuffers.clear();
});

function storeEditors(projectName) {
  const projectPane = projectPanes[projectName];
  if (!projectPane) return;

  const serializedData = {
    // [[uri, index, cursorPosition], ...]
    editorsList: [],
    activeItemIndex: projectPane.getActiveItemIndex()
  }
  // populate editorsList;
  projectPane.getItems().forEach((item, index) => {
    const uri = typeof item.getURI === 'function' && item.getURI();
    const cursorPosition = typeof item.getCursorBufferPosition === 'function' &&
          item.getCursorBufferPosition();
    if (uri && cursorPosition)
      serializedData.editorsList.push([uri, index, cursorPosition]);
  });

  const data = JSON.stringify(serializedData);
  const storePath = path.join(projectsDir, projectName, '.cache/state/editors');
  fs.writeFile(storePath, data, (err) => {
    if (err) fs.mkdir(path.dirname(storePath), { recursive: true }, (err) => {
      if (err) { console.error(err) } else {
        fs.writeFile(storePath, data, (err) => { if (err) console.error(err); })
      }
    });
  });
}

const projectsWithChangedEditors = new Set();
setInterval(() => {
  projectsWithChangedEditors.forEach(projectName => storeEditors(projectName));
  projectsWithChangedEditors.clear();
}, 15000);
atom.window.addEventListener('beforeunload', (_) => {
  projectsWithChangedEditors.forEach(projectName => storeEditors(projectName));
  projectsWithChangedEditors.clear();
});

function restoreBuffers(projectName) {
  const storePath = path.join(projectsDir, projectName, '.cache/state/buffers');
  let data;
  try { data = fs.readFileSync(storePath) }
  catch (_) { return }

  let serializedBuffers;
  try { serializedBuffers = JSON.parse(data) }
  catch (err) { console.error(err); return }

  serializedBuffers.forEach(serializedBuffer => {
    if (serializedBuffer.shouldDestroyOnFileDelete == null) {
      serializedBuffer.shouldDestroyOnFileDelete =
        () => atom.config.get('core.closeDeletedFileTabs');
    }
    // use a little guilty knowledge of the way TextBuffers are serialized;
    // this allows TextBuffers that have never been saved (but have filePaths) to be deserialized,
    //   but prevents TextBuffers backed by files that have been deleted from being saved;
    serializedBuffer.mustExist = serializedBuffer.digestWhenLastPersisted !== false;

    require('atom').TextBuffer.deserialize(serializedBuffer)
      .then(buffer => {
        atom.project.grammarRegistry.maintainLanguageMode(buffer);
        atom.project.subscribeToBuffer(buffer);
      })
      .catch(err => {
        delete atom.project.loadPromisesByPath[uri];
        console.error(err);
      });
  });
}

function restoreProject(projectName) {
  restoreBuffers(projectName);

  const projectPane = projectPanes[projectName];
  if (!projectPane) return;

  const storePath = path.join(projectsDir, projectName, '.cache/state/editors');
  let data;
  try { data = fs.readFileSync(storePath) }
  catch (_) { atom.workspace.paneForURI('atom://tree-view').activate(); return }

  let serializedEditors;
  try { serializedEditors = JSON.parse(data) }
  catch (err) { console.error(err); return }

  serializedEditors.editorsList.reduce((p, [uri, index, cursorPosition]) =>
    p.then(() =>
      atom.workspace.createItemForURI(uri).then(item => {
        projectPane.addItem(item, {index});
        typeof item.setCursorBufferPosition === 'function' &&
          item.setCursorBufferPosition(cursorPosition);
      })
    ),
    Promise.resolve()
  ).then(() => {
    projectPane.activateItemAtIndex(serializedEditors.activeItemIndex);
    // if there is no item in projectPane, focus tree-view;
    if (projectPane.getItems().length == 0) {
      atom.workspace.paneForURI('atom://tree-view').activate();
    }
  });
}

atom.workspace.getCenter().observeTextEditors(editor => {
  const projectName = path.relative(projectsDir, atom.project.getPaths()[0]);
  // check if editor is in "projectPanes[projectName]",
  //   if not search all panes, and find the right projectName;

  projectsWithChangedEditors.add(projectName);
  editor.onDidDestroy(() => {
    projectsWithChangedEditors.add(projectName);
    projectsWithChangedBuffers.add(projectName);
  });
  editor.onDidChangeCursorPosition(_ => projectsWithChangedEditors.add(projectName));

  const buffer = editor.getBuffer();
  buffer.onDidStopChanging(() => projectsWithChangedBuffers.add(projectName));
  // not sure if this is necessary; does saving imply changing?
  buffer.onDidSave((_) => projectsWithChangedBuffers.add(projectName));
});

class ProjectsList {
  constructor() {
    this.modalPanel = null;
    this.previouslyFocusedElement = null;
    this.selectList = new SelectList({
      items: [],

      elementForItem: (item) => {
        const li = document.createElement('li');
        const span = document.createElement('span');
        span.textContent = item;
        li.appendChild(span);
        return li;
      },

      didConfirmSelection: (projectName) => {
        this.selectList.reset();
        this.modalPanel.hide();
        atom.project.setPaths([path.join(projectsDir, projectName)]);

        let projectPane = projectPanes[projectName];
        if (!projectPane) {
          projectPane = atom.workspace.getCenter().getActivePane().splitRight();
          projectPane.onWillDestroy(() => {
            delete projectPanes[projectName];
            atom.project.setPaths([]);
            this.show();
          });
          projectPanes[projectName] = projectPane;
          restoreProject(projectName);
        }

        // hide all panes, show only the selected project pane, and activate it;
        atom.workspace.getCenter().getPanes().forEach(pane => {
          const view = atom.views.getView(pane);
          view.style.display = 'none';
        });
        {
          const view = atom.views.getView(projectPane);
          view.style.display = '';
        }
        projectPane.activate();

        this.selectList.update(
          { initialSelectionIndex: this.selectList.items.indexOf(projectName) }
        );
      },

      didCancelSelection: () => {
        this.selectList.reset();
        this.modalPanel.hide();
        if (this.previouslyFocusedElement) {
          this.previouslyFocusedElement.focus();
          this.previouslyFocusedElement = null;
        }
      }
    });
  }

  show() {
    this.previouslyFocusedElement = document.activeElement;

    fs.readdir(projectsDir, (err, fileNames) => {
      if (err) { alert(err.message); }
      else {
        const projectNames = fileNames.filter(
          fileName => fs.statSync(path.join(projectsDir, fileName)).isDirectory()
        );
        this.selectList.update({ items: projectNames });
      }
    });

    if (!this.modalPanel) {
      this.modalPanel = atom.workspace.addModalPanel({ item: this.selectList});
    }
    this.modalPanel.show();
    this.selectList.focus();
  }

  createNewProject() {}
}

const projectsList = new ProjectsList();
projectsList.show();

atom.commands.add('atom-workspace', {
  'comshell:projects-list': () => {
    if (projectsList.modalPanel.isVisible()) {
      projectsList.selectList.props.didCancelSelection();
    } else {
      projectsList.show();
    }
  }
});

// to define keybindings for projectsList, add a class:
projectsList.selectList.element.classList.add('projects-list');

require('./status-bar');
require('./tree-view');

// https://github.com/alexfu/atom-replace-pane
// https://github.com/atom/settings-view/blob/master/lib/package-manager.coffee
// modal key bindings:
//   https://github.com/Kesin11/atom-vim-like-tab/#keymap
// https://atom.io/packages/structure-view
// https://atom.io/packages/simple-git
// https://atom.io/packages/git-plus
// https://atom.io/packages/atom-video

// https://medium.com/hacking-atom/tweak-your-atom-s-init-script-without-reloading-atom-with-a-declarative-module-8b1c0f208663

// 2 spaces -> enter
atom.commands.add('atom-text-editor', 'comshell:space', () => {
  const editor = atom.workspace.getActiveTextEditor(); // WRONG
  const cursor = editor.getLastCursor();

  if (cursor.hasPrecedingCharactersOnLine()) {
    editor.setText(' ');
  } else if (cursor.isAtBeginningOfLine()) {
    editor.setText('\n');
  } else {
    editor.backspace();
    editor.setText('\n');
  }
})

/* importing from a package (probably a bad idea)
atom.packages.activatePackage("tree-view").then((pkg) => {
  if (pkg && pkg.mainModule && pkg.mainModule.treeView) {
    treeView = pkg.mainModule.treeView;

    if (!treeView.roots[0] || !treeView.roots[0].directory) {
      atom.project.onDidChangePaths(() => {});
      return;
    }
  }},

  (reason) => {
    atom.notifications.addWarning('failure', {
      description: reason.message
    });
  }
);
*/
