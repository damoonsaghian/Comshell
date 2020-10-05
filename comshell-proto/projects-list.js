const path = require('path');
const fs = require('fs');

const projectsList = document.createElement('dialog');
module.exports = projectsList;

window.addEventListener('keydown', (key) => {
  if (key === '')
    projectsList.showModal();
  if (key === '')
    projectsList.close();
});

const projectsDirs = [];
addProjectsDir('');

// when a disk is mounted, add its "projects" directory to the list, and update;

function addProjectsDir(projectsDir) {
  this.projectsDir.walkDir((file) => {});
}

function activateProject(projectName) {
  const command =
    nwExecPath + ' --user-data-dir="' +
    path.join(this.projectsDir, projectName, '.cache/chromium') + '" ' +
    __dirname;
  require('child_process').exec(
    command,
    (error, _stdout, _stderr) => {
      // if this is a project_less instance (ie the first instance), close it;
      if (!error && !atom.projectRootPath)
        nw.Window.get().close();
    }
  );
}

function onProjectActivated(callback) {
  // hide overview when a project is activated;
  callback(this.selectedProjectDir);
}

module.exports = projectsList;
