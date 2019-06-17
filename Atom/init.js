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
        atom.open({ pathsToOpen: [projectPath] });
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
    //this.previouslyFocusedElement = document.activeElement;

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

//require('./state-store');
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
