const fs = require('fs');
const path = require('path');
const SelectList = global.require('atom-select-list');

atom.workspace.observeTextEditors(editor => {
  const grammar = editor.getGrammar();
  if (grammar.name === "Plain Text" || grammar === atom.grammars.nullGrammar)
    atom.textEditors.setGrammarOverride(editor, 'text.plain');
});

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

atom.enablePersistence = false;

// { 'project name': projectPane }
const projectPanes = {};

function storeProjectBuffers(projectName) {
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
  projectsWithChangedBuffers.forEach(projectName => storeProjectBuffers(projectName));
  projectsWithChangedBuffers.clear();
}, 15000);
atom.window.addEventListener('beforeunload', (_) => {
  projectsWithChangedBuffers.forEach(projectName => storeProjectBuffers(projectName));
  projectsWithChangedBuffers.clear();
});

function restoreBuffers(projectName) {
  const storePath = path.join(projectsDir, projectName, '.cache/atom-buffers/');
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

function storeProjectPane(projectName) {
  const projectPane = projectPanes[projectName];
  if (!projectPane) return;

  const serializedPane = ;

  const data = JSON.stringify(serializedPane);
  const storePath = path.join(projectsDir, projectName, '.cache/atom-pane');
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

function restoreProjectPane(projectName) {
  restoreBuffers(projectName);

  const storePath = path.join(projectsDir, projectName, '.cache/atom-pane');
  let data;
  try { data = fs.readFileSync(storePath) }
  catch (_) { atom.workspace.paneForURI('atom://tree-view').activate(); return }

  let serializedPane;
  try { serializedPane = JSON.parse(data) }
  catch (err) { console.error(err); return }

  let projectPane = require('atom').Pane.deserialize(serializedPane, atom);
  let activePane = atom.workspace.getCenter().getActivePane();
  activePane.parent.insertChildAfter(activePane, projectPane);

  projectPane.activate();
  projectPane.onWillDestroy(() => {
    delete projectPanes[projectName];
    atom.project.setPaths([]);
  });
  projectPanes[projectName] = projectPane;

  // if there is no item in projectPane, focus tree-view;
  if (projectPane.getItems().length == 0) {
    atom.workspace.paneForURI('atom://tree-view').activate();
  }

  projectPane.onDidMoveItem(_ => projectsWithChangedEditors.add(projectName));
  projectPane.onDidRemoveItem(_ => projectsWithChangedEditors.add(projectName));
  projectPane.onDidAddItem(_ => projectsWithChangedEditors.add(projectName));

  projectPane.observeItems(item => {
    projectsWithChangedEditors.add(projectName);
    if (item instanceof require('tom').TextEditor) {
      editor.onDidDestroy(() => {
        projectsWithChangedEditors.add(projectName);
        projectsWithChangedBuffers.add(projectName);
      });
      editor.onDidChangeCursorPosition(_ => projectsWithChangedEditors.add(projectName));

      const buffer = editor.getBuffer();
      buffer.onDidStopChanging(() => projectsWithChangedBuffers.add(projectName));
      // not sure if this is necessary; does saving imply changing?
      buffer.onDidSave((_) => projectsWithChangedBuffers.add(projectName));
    }
  });
}

class ProjectsList {
  constructor() {
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
        const projectPath = path.join(projectsDir, projectName);
        const projectPane = projectPanes[projectName];

        atom.project.setPaths([path.join(projectsDir, projectName)]);

        if (!projectPane) {
          restoreProjectPane(projectName);
        } else {
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
        }

      },

      didCancelSelection: () => {
        this.selectList.reset();
        this.modalPanel.hide();
        atom.workspace.getCenter().getActivePane().activate();
      }
    });

    this.modalPanel = atom.workspace.addModalPanel({ item: this.selectList, visible: false});
  }

  show() {
    fs.readdir(projectsDir, (err, fileNames) => {
      if (err) { alert(err.message); return }

      const projectNames = fileNames.filter(
        fileName => fs.statSync(path.join(projectsDir, fileName)).isDirectory()
      );
      const currentProjectPath = atom.project.getPaths()[0];
      const currentProjectName = currentProjectPath ?
        path.relative(projectsDir, currentProjectPath) : null;

      this.selectList.update({
        items: projectNames,
        initialSelectionIndex:
          currentProjectName ? projectNames.indexOf(currentProjectName) : 0
      });
    });

    this.modalPanel.show();
    this.selectList.focus();
  }

  createNewProject() {}
}

const projectsList = new ProjectsList();
if (!atom.project.getPaths()[0]) projectsList.show();

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

atom.commands.add('atom-text-editor', 'comshell:space', () => {
})
atom.commands.add('atom-text-editor', 'comshell:comma', () => {
})

// "https://github.com/alexfu/atom-replace-pane"

// "https://github.com/sh8/web-lookup/blob/master/lib/web-lookup.coffee"
//   "https://github.com/sh8/web-lookup/blob/master/lib/web-lookup-view.coffee"
// "https://github.com/skandasoft/browser-plus/blob/master/package.json"
//   "https://github.com/skandasoft/browser-plus/tree/master/lib"
// "https://github.com/skandasoft/browser-plus-open-new-window"
// "https://github.com/nju33/atom-pane-browser"
// "https://github.com/sean-codes/atom-browser"
// "https://atom.io/packages/Navigate"

// "https://atom.io/packages/simple-git"
// "https://atom.io/packages/git-plus"

// "https://github.com/atom/settings-view/blob/master/lib/package-manager.coffee"

// modal key bindings:
// "https://github.com/Kesin11/atom-vim-like-tab/#keymap"

// "https://atom.io/packages/structure-view"

// "https://atom.io/packages/quick-file-browser"
// "https://atom.io/packages/atom-video"

// "https://atom.io/packages/plain-simple"
// "https://medium.com/hacking-atom/tweak-your-atom-s-init-script-without-reloading-atom-with-a-declarative-module-8b1c0f208663"

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
