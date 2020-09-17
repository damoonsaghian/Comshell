const path = require('path');
const fs = require('fs');

module.exports =
class ProjectsList extends HTMLElement {
  projectsDirs;

  constructor() {
    this.projectsDirs = [];
    // when a disk is mounted, add its "projects" directory to the list;

    this.projectsDirs.forEach((projectsDir) => this.projectsDir.walkDir(
      (file) => {}
    ));
  }

  activateProject(projectName) {
    const command =
      nwExecPath + ' --user-data-dir ' +
      path.join(this.projectsDir, projectName, '.cache/atom') + ' ' +
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

  onProjectActivated(callback) {
    // hide overview when a project is activated;

    callback(this.selectedProjectDir);
  }
}
