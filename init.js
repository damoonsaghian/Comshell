atom.enablePersistence = false;

const fs = require('fs');
const path = require('path');
const _ = require('/usr/lib/atom/node_modules/underscore');
const SelectList = require('/usr/lib/atom/node_modules/atom-select-list');

const projectsDir = path.join(require('os').homedir(), 'projects');
// if "~/projects/" directory does not exist, create it;
fs.stat(projectsDir, (err, stats) => {
  if (err) {
    fs.mkdir(projectsDir, (err) => { if (err) alert(err.message); });
  }
  else if (!stats.isDirectory()) {
    alert('"projects" directory can\'t be created, because there is a file with the same name');
  }
});

// { 'project name': { editors: [], items: []}
const openProjects = {};
const changedProjects = new Set();
const changedBuffers = new Set();

class ProjectsList {
  constructor() {
    this.modalPanel = null;
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
        // remove all items in the center workspace;
        atom.workspace.getCenter().getPanes().forEach(pane => {
          pane.getItems().forEach(item => pane.removeItem(item));
        });
        atom.project.setPaths([path.join(projectsDir, projectName)]);

        const activePane = atom.workspace.getCenter().getActivePane();

        if (openProjects[projectName]) {
          activePane.addItems(openProjects[projectsName].items);
        } else {
          restoreEditors(projectName);
        }

        // if there is no open item, focus tree-view;
        if (activePane.getItems().length == 0) {
          try { document.querySelector('.tree-view').focus(); }
        }

        this.selectList.update(
          { initialSelectionIndex: this.selectList.items.indexOf(projectName) }
        );
      },

      didCancelSelection: () => {
        this.selectList.reset();
        this.modalPanel.hide();
      }
    });
  }

  show() {
    fs.readdir(projectsDir, (err, fileNames) => {
      if (err) { alert(err.message); }
      else {
        let projectNames = fileNames.filter(fileName => {
          fs.statSync(path.join(projectsDir, fileName)).isDirectory()
        });
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
  'comshell:projects-list': () => projectsList.show()
});

// to define keybindings for projectsList, add a class:
projectsList.selectList.element.classList.add('projects-list');

function storeUnsaved(buffer) {
  if (!buffer || buffer.isDestroyed()) return;
  const data = JSON.stringify('unsaved portion of buffer');
  fs.writeFile(buffer.getPath() + '.unsaved', data, err => { if (err) throw err; });
}

function restoreUnsaved(buffer) {
  if (!buffer || buffer.isDestroyed()) return;
  fs.readFile(buffer.getPath() + '.unsaved', (err, data) => {
    if (err) return;
    try {
      const serializedData = JSON.parse(data);
      // restore unsaved data
    }
    catch (err) { throw err }
  });
}

function storeEditors(projectName) {
  const paneStorePath = path.join(projectsDir, projectName, '.cache/pane');
  openProjects[projectName].editors.forEach();
  const items = atom.getTextEditors().forEach(editor => {
    const editorPath = editor.getPath();
    const cursorPosition = editor.getCursorBufferPosition();
    return [uri, cursorPosition];
  })
  const data = JSON.stringify([/*list of paths and cursor position of open editors*/]);
  fs.writeFile(paneStorePath, data, (err) => {
    if (err) fs.mkdir(path.dirname(paneStorePath), { recursive: true }, (err) => {
      if (err) fs.writeFile(paneStorePath, data, () => { if (err) throw err; })
    })
  });
}

function restoreEditors(projectName) {
  const paneStorePath = path.join(projectsDir, projectName, '.cache/pane');
  if (pane.isDestroyed()) return;
  fs.readFile(paneStorePath, (err, data) => {
    if (err) return;
    try {
      const serializedData = JSON.parse(data);
      // open the editors with the given paths, inside this pane, and restore their cursor position;
    }
    catch (err) { throw err }
  });
}

atom.workspace.getCenter().observeTextEditors(editor => {
  const projectName = path.relative(projectsDir, atom.project.getPaths()[0]);
  openProjects[projectName].editors.push(editor);
  editor.onWillDestroy(() => delete openProjects[projectName].editors[editor]);
  changedProjects.add(projectName);

  const buffer = editor.getBuffer();
  restoreUnsaved(buffer);

  editor.onDidStopChanging(() => changedBuffers.add(buffer));
  editor.onDidChangeBuffer(() => changedProjects.add(projectName));
  editor.onDidChangeCursorPosition(event => changedProjects.add(projectName));
});

setInterval(() => changedProjects.forEach(projectName => {
  storeEditors(projectName);
  changedProjects.delete(projectName);
}), 30000);

setInterval(() => changedBuffers.forEach(buffer => {
  storeBuffer(buffer);
  changedBuffers.delete(buffer);
}), 30000);

// when an item appears, push it to the items of current open project;
atom.workspace.getCenter().observePaneItems(item => {
  const projectName = path.relative(projectsDir, atom.project.getPaths()[0]);
  openProjects[projectName].items.push(item);
});

atom.workspace.getCenter().onWillDestroyPaneItem(event => {
  const projectName = path.relative(projectsDir, atom.project.getPaths()[0]);
  delete openProjects[projectName].items[event.item];
});

// https://atom.io/packages/tree-view-auto-collapse
// https://atom.io/packages/tree-view-scope-lines
// https://atom.io/packages/tree-lines
// https://github.com/hswolff/tree-view-extended/tree/master/lib
// https://atom.io/packages/tree-view-git-status
// https://atom.io/packages/file-icons

// https://github.com/alexfu/atom-replace-pane
// https://atom.io/packages/atom-clock
// https://github.com/atom/settings-view/blob/master/lib/package-manager.coffee
// https://atom.io/packages/structure-view
// https://atom.io/packages/simple-git
// https://atom.io/packages/git-plus

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
