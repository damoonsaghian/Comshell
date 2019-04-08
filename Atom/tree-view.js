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

  atom.workspace.getCenter().onDidStopChangingActivePaneItem(() => {
    treeView.roots.forEach(root => {
      root.collapse(true);
      root.expand(false);
    });
    treeView.revealActiveFile({show: false, focus: false});
    // to do: change this function to open gallery directories, instead of expanding them;
    // https://github.com/atom/tree-view/blob/master/lib/tree-view.coffee#L364
  })
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
