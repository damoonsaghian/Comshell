// one pane per project
// only the active project's pane is displayed, others will be hidden;
// panes are stored/restored to/from a file in the ".cache" directory of each project;

atom.enablePersistence = false;

const fs = require('fs');
const path = require('path');
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

function storeUnsaved(buffer) {
  if (!buffer || buffer.isDestroyed()) return;
  const data = JSON.stringify('unsaved portion of buffer');
  //fs.writeFile(buffer.getPath() + '.unsaved', data, err => { if (err) throw err; });
}

function restoreUnsaved(buffer) {
  if (!buffer || buffer.isDestroyed()) return;
  fs.readFile(buffer.getPath() + '.unsaved', (err, data) => {
    if (err) return;
    try {
      const serializedData = JSON.parse(data);
      // restore unsaved portion of buffer;
    }
    catch (err) { throw err }
  });
}

setInterval(() => {
  changedBuffers.forEach(buffer => storeUnsaved(buffer));
  changedBuffers.clear();
}, 30000);

// { 'project name': projectPane }
const projectPanes = {};

function storeProjectState(projectName) {
  const projectPane = projectPanes[projectName];
  if (!projectPane) return;

  const serializedData = {
    // list of paths and cursor position of open editors;
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
  })
  
  const data = JSON.stringify(serializedData);
  const storePath = path.join(projectsDir, projectName, '.cache/project_state');
  fs.writeFile(storePath, data, (err) => {
    if (err) fs.mkdir(path.dirname(storePath), { recursive: true }, (err) => {
      if (err) { throw err } else {
        fs.writeFile(storePath, data, (err) => { if (err) throw err; })
      }
    });
  });
}

function restoreProjectState(projectName) {
  const projectPane = projectPanes[projectName];
  if (!projectPane) return;

  const storePath = path.join(projectsDir, projectName, '.cache/project_state');
  fs.readFile(storePath, (err, data) => {
    if (err) {
      atom.commands.dispatch(atom.views.getView(atom.workspace.element), 'tree-view:toggle-focus');
      return;
    }
    
    try {
      const serializedData = JSON.parse(data);

      serializedData.editorsList.reduce((p, [uri, index, cursorPosition]) =>
        p.then(() =>
          atom.workspace.createItemForURI(uri).then(item => {
            projectPane.addItem(item, {index});
            typeof item.setCursorBufferPosition === 'function' &&
              item.setCursorBufferPosition(cursorPosition);
          })
        )
        , Promise.resolve()).then(() => {
        projectPane.activateItemAtIndex(serializedData.activeItemIndex);
        // if there is no item in projectPane, focus tree-view;
        if (projectPane.getItems().length == 0) {
          atom.commands.dispatch(atom.views.getView(atom.workspace.element), 'tree-view:toggle-focus');
        }
      });
    }
    catch (err) { throw err }
  });
}

setInterval(() => {
  changedProjects.forEach(projectName => storeProjectState(projectName));
  changedProjects.clear();
}, 30000);

const changedProjects = new Set();
const changedBuffers = new Set();

atom.workspace.getCenter().observeTextEditors(editor => {
  const buffer = editor.getBuffer();
  restoreUnsaved(buffer);

  const projectName = path.relative(projectsDir, atom.project.getPaths()[0]);
  changedProjects.add(projectName);

  editor.onDidChangeCursorPosition(event => changedProjects.add(projectName));
  editor.onDidStopChanging(() => changedBuffers.add(buffer));
});

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
          restoreProjectState(projectName);
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
      }
    });
  }

  show() {
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
  'comshell:projects-list': () => projectsList.show()
});

// to define keybindings for projectsList, add a class:
projectsList.selectList.element.classList.add('projects-list');

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
