// one pane per project
// in center workspace, only the active pane is displayed, others will be hidden;
// panes are stored/restored to/form a file in the ".cache" directory of each project;

// hiding panes is done by setting their CSS display property of inactive panes, to none;
// this method can be problematic;
// eg when using the project mechanism of Atom the initial pane will not have "active" property,
//   until another pane is added;
// since we do not use project mechanism, we can ignore this;
// nonetheless here is an alternative method:
//   

const fs = require('fs');
const path = require('path');
const SelectList = require('/usr/lib/atom/node_modules/atom-select-list');

const projectsDir = path.join(require('os').homedir(), 'projects');
// if "~/projects/" directory does not exist, create it;
fs.stat(projectsDir, (err, stats) => {
  if (err) {
    fs.mkdir(projectsDir, (err) => {
      if (err) { alert(err.message); }
    });
  }
  else if (!stats.isDirectory()) {
    alert('"projects" directory can\'t be created, because there is a file with the same name');
  }
});

atom.enablePersistence = false;

// { 'project name': projectPane }
const projectPanes = {};

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

      didConfirmSelection: (item) => {
        // item is actually a project's name;
        this.selectList.reset();
        atom.project.setPaths([path.join(projectsDir, item)]);

        if (!(item in projectPanes)) {
          let newPane = atom.workspace.getCenter().getActivePane().splitRight();
          projectPanes[item] = newPane;
          // focus tree-view
          atom.commands.dispatch(atom.views.getView(atom.workspace.element), 'tree-view:toggle-focus');
        }

        // hide all panes, show only the selected project pane, and activate it;
        atom.workspace.getCenter().getPanes().forEach((pane) => {
          const view = atom.views.getView(pane);
          view.style.display = 'none';
        });
        const projectPane = projectPanes[item];
        const view = atom.views.getView(projectPane);
        view.style.display = '';
        if (projectPane.getItems().length != 0) { projectPane.activate(); }

        this.selectList.update(
          { initialSelectionIndex: this.selectList.items.indexOf(item) }
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
        let projectNames = fileNames.filter((fileName) =>
                                            fileName[0] != '.' &&
                                            fs.statSync(path.join(projectsDir, fileName)).isDirectory());
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

// store/restore workspace to/from a config field:
// https://github.com/denieler/save-workspace-atom-plugin/blob/master/lib/models/workspace.js

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
