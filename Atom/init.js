// one pane per project
// panes are stored/restored to/form a file in the ".cache" directory of each project;
// in center workspace, only the active pane is displayed, others will be hidden
//   (by setting their CSS display property, to none);

// projects list view:
// https://github.com/chocoelho/project-switcher2/blob/master/lib/utils.coffee
// https://github.com/chuckhendo/project-quick-open/blob/master/lib/project-quick-open-view.coffee

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

// https://stackoverflow.com/questions/35620764/how-to-disable-alert-dialogs-when-errors-occur-in-atom-electron
// https://discuss.atom.io/t/how-to-disable-alert-dialogs-when-errors-occur/20037/2

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
