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

const fs = require("fs");
const SelectList = require("atom-select-list");

// { "project name": projectPane }
var projectPanes = {};

const projectsList = {
  selectList: new SelectList({
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
      // change root dir in the tree_view;

      if (!(item in projectPanes)) {
        projectPanes[item] = atom.workspace.getCenter().getActivePane().splitRight();
        // focus tree_view;
        atom.commands.dispatch(atom.views.getView(atom.workspace), 'tree_view:toggle-focus')
      }
      // hide all panes, show only the the selected project pane, and activate it;
    },

    didCancelSelection: () => {
      this.selectList.reset();
      this.modalPanel.hide();
    }
  }),

  modalPanel: null,

  show() {
    // if "~/projects/" directory does not exist, create it;
    fs.readdir("~/projects/", (err, fileNames) => {
      if (err) { alert(err.message); }
      else {
        let projectNames = fileNames.filter((fileName) =>
                                            fileName[0] != '.' &&
                                            fs.statSync("~/projects/" + fileName).isDirectory());
        this.selectList.update({ items: projectNames });
      }
    });
        
    if (!this.panel) {
      this.modalPanel = atom.workspace.addModalPanel({ item: this.selectList , autoFocus: true });
    }
    this.panel.show();
    //this.selectList.focus();
  },

  createNewProject() {}
};

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

function getFileExtension(filename) {
  return filename.slice((filename.lastIndexOf('.') - 1 >>> 0) + 2);
}

function getFileName(filename) {
  return filename.slice(0, (filename.lastIndexOf('.') - 1 >>> 0) + 1);
}

// https://github.com/alexfu/atom-replace-pane
// https://atom.io/packages/atom-clock
// https://github.com/atom/settings-view/blob/master/lib/package-manager.coffee
// https://atom.io/packages/structure-view
// https://atom.io/packages/simple-git
// https://atom.io/packages/git-plus

// https://medium.com/hacking-atom/tweak-your-atom-s-init-script-without-reloading-atom-with-a-declarative-module-8b1c0f208663

// 2 spaces -> enter
atom.commands.add('atom-text-editor', 'comshell:space', () => {
  const editor = atom.workspace.getActiveTextEditor();
  const cursor = editor.getLastCursor();

  if (cursor.hasPrecedingCharactersOnLine()) {
    editor.setText(" ");
  } else if (cursor.isAtBeginningOfLine()) {
    editor.setText("\n");
  } else {
    editor.backspace();
    editor.setText("\n");
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
