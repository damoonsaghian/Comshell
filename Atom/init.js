// one pane per project
// in center workspace, only the active pane is displayed, others will be hidden,
//   by setting their CSS display property, to none:
// panes are stored/restored to/form a file in the ".cache" directory of each project;

// projects list:
// https://github.com/chocoelho/project-switcher2/blob/master/lib/utils.coffee
const fs = require("fs");
const SelectList = require("atom-select-list");
const projectsList = new SelectList({ items: [] });
// if "~/projects/" directory does not exist, create it;
fs.readdir("~/projects/", (err, paths) => {
  if (err) { alert(err.message); }
  else {
    let projectPaths = paths.filter(paths =>
                                    path[0] != '.' &&
                                    fs.existsSync("~/projects/" + file) &&
                                    fs.statSync("~/projects/" + file).isDirectory());
    // set items, and populate list
  }
});
// projectsList.element
// on confirmed:
// , change root dir in the tree_view,
// , if there is a pane named "project_name" switch to it, otherwise create it, and focus tree_view

// store/restore workspace to/from a config field:
// https://github.com/denieler/save-workspace-atom-plugin/blob/master/lib/models/workspace.js

// https://atom.io/packages/tree-view-git-status
// https://atom.io/packages/tree-view-scope-lines
// https://atom.io/packages/tree-lines
// https://atom.io/packages/tree-view-auto-collapse
// https://github.com/hswolff/tree-view-extended/tree/master/lib
// https://atom.io/packages/file-icons

// https://github.com/alexfu/atom-replace-pane
// https://atom.io/packages/atom-clock
// https://github.com/atom/settings-view/blob/master/lib/package-manager.coffee
// https://atom.io/packages/simple-git
// https://atom.io/packages/git-plus

// https://medium.com/hacking-atom/tweak-your-atom-s-init-script-without-reloading-atom-with-a-declarative-module-8b1c0f208663

const workspaceCenter = atom.workspace.getCenter();

function gotoProject(uri) {
  let pane = workspaceCenter.getPanes().find(item => {
    return item.getUri
  });
  if (!pane) {
    workspaceCenter().getActivePane().splitRight();
    // open uri
  }
}

atom.commands.add('atom-text-editor', 'custom:space', () => {
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

function getFileExtension(filename) {
  return filename.slice((filename.lastIndexOf('.') - 1 >>> 0) + 2);
}

function getFileName(filename) {
  return filename.slice(0, (filename.lastIndexOf('.') - 1 >>> 0) + 1);
}

atom.packages.activatePackage("tree-view").then((pkg) => {
  if (pkg && pkg.mainModule && pkg.mainModule.treeView) {
    treeView = pkg.mainModule.treeView;

    if (!treeView.roots[0] || !treeView.roots[0].directory) {
      atom.project.onDidChangePaths(() => {});
      return;
    }
  }},
  (reason) => {
    atom.notifications.addWarning("failure", {
      description: reason.message
    });
  }
);
