if (atom.packages.isPackageActive('tree-view')) {
  const treeView = atom.packages.getActivePackage('tree-view').mainModule.getTreeViewInstance();
  if (treeView) { init(treeView) }
}
atom.packages.onDidActivatePackage(activatedPackage => {
  if (activatedPackage === atom.packages.getActivePackage('tree-view')) {
    const treeView = activatedPackage.mainModule.getTreeViewInstance();
    if (treeView) { init(treeView) }
  }
});

function init(treeView) {
  // remove disclosure arrows in tree_view;
  treeView.list.classList.remove('has-collapsable-children');

  // numerical sorting: 10 after 9;
  //treeView.roots[0].directory.constructor.prototype.sortEntries = sortEntries;

  // collapse all, then reveal the active item;
  atom.workspace.getCenter().onDidStopChangingActivePaneItem(() => {
    treeView.collapseDirectory(true, true);
    treeView.revealActiveFile({show: false, focus: false});
  })
  atom.project.onDidChangePaths(() => {
    treeView.collapseDirectory(true, true);
    treeView.revealActiveFile({show: false, focus: false});
  });
}

function sortEntries(combinedEntries) {
  // https://github.com/hex-ci/atom-tree-view-sort/blob/master/lib/tree-view-sort.js#L158
  return combinedEntries.sort((first, second) => {
    const firstName = first.name ? first.name.toLowerCase() : first;
    const secondName = second.name ? second.name.toLowerCase() : second;
    return firstName.localeCompare(secondName)
  });
}

// https://atom.io/packages/tree-view-scope-lines
// https://atom.io/packages/tree-lines
// https://github.com/hswolff/tree-view-extended/tree/master/lib
// https://atom.io/packages/tree-view-git-status
// https://atom.io/packages/file-icons
