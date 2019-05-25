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
    this.modalPanel = null;
    //this.previouslyFocusedElement = null;
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
        //if (this.previouslyFocusedElement) {
        //  this.previouslyFocusedElement.focus();
        //  this.previouslyFocusedElement = null;
        //}
      }
    });
  }

  show() {
    //this.previouslyFocusedElement = document.activeElement;

    fs.readdir(projectsDir, (err, fileNames) => {
      if (err) { alert(err.message); }
      else {
        const projectNames = fileNames.filter(
          fileName => fs.statSync(path.join(projectsDir, fileName)).isDirectory()
        );
        this.selectList.update({ items: projectNames });
      }
    });

    const projectPath = atom.project.getPaths()[0];
    if (projectPath) this.selectList.update(
      {
        initialSelectionIndex: this.selectList.items.indexOf(
          path.relative(projectsDir, projectPath)
        )
      }
    );

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
if (atom.project.getPaths()[0])
  projectsList.selectList.props.didCancelSelection();

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
