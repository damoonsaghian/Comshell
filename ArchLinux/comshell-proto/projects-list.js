const fs = require('fs');
const path = require('path');
const SelectList = global.require('atom-select-list');

const projectsDir = path.join(require('os').homedir(), 'projects');
// if "~/projects/" directory does not exist, create it;
fs.stat(projectsDir, (err, stats) => {
  if (err) {
    fs.mkdir(projectsDir, (err) => { if (err) alert(err.message); });
  } else if (!stats.isDirectory()) {
    alert('can\'t create projects directory, cause there is a file with the same name');
  }
});

const nwExecPath = require('process').execPath;

module.exports =
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

        const command =
          nwExecPath + ' --mixed-context ' +
          '--user-data-dir ' + path.join(projectsDir, projectName, '.cache/atom') + ' ' +
          __dirname;
        require('child_process').exec(
          command,
          (error, _stdout, _stderr) => {
            // if this is a project_less instance (ie the first instance), close it;
            if (!error && !atom.projectRootPath)
              nw.Window.get().close();
          }
        );
      },

      didCancelSelection: () => {
        this.selectList.reset();
        this.modalPanel.hide();
        atom.workspace.getCenter().getActivePane().activate();
      }
    });

    this.modalPanel = atom.workspace.addModalPanel({ item: this.selectList, visible: false });
  }

  show() {
    fs.readdir(projectsDir, { withFileTypes: true }, (err, dirents) => {
      if (err) {
        alert(err.message);
        return;
      }

      const projectNames = dirents
        .filter(dirent => dirent.isDirectory())
        .map(dirent => dirent.name);
      const currentProjectPath = atom.project.getPaths()[0];
      const currentProjectName = currentProjectPath ?
        path.relative(projectsDir, currentProjectPath) : null;

      this.selectList.update({
        items: projectNames,
        initialSelectionIndex:
          currentProjectName ? projectNames.indexOf(currentProjectName) : 0
      });

      this.modalPanel.show();
      this.selectList.focus();
    });
  }

  createNewProject() {}
}
