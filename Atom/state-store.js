// the state of a project's pane, is stored/restored to/from files
//   in ".cache/atom_state" directory of each project;

const path = require('path');

class StateStore {
  constructor () {
    const currentProjectPath = atom.project.getPaths()[0];
    this.storeFilePath = currentProjectPath ?
      path.join(currentProjectPath, '.cache/atom_state') : null;
    this. = atom
  }

}

function storeState() {
  const data = JSON.stringify(atom.serialize());
  const storePath = path.join(atom.project.getPaths()[0], '.cache/atom_state');
  fs.writeFile(storePath, data, (err) => {
    if (err) fs.mkdir(path.dirname(storePath), { recursive: true }, (err) => {
      if (err) { console.error(err) } else {
        fs.writeFile(storePath, data, (err) => { if (err) console.error(err); })
      }
    });
  });
}

function restoreState() {
  const storePath = path.join(atom.project.getPaths()[0], '.cache/atom_state');
  fs.readFile(storePath, (err, data) => {
    if (err) {
      atom.commands.dispatch(atom.views.getView(atom.workspace.element), 'tree-view:toggle-focus');
      return;
    }

    try {
      const serializedData = JSON.parse(data);

      serializedData.editorsList.reduce((p, [uri, index, cursorPosition]) =>
        p.then(() =>
          atom.workspace.createItemForURI(uri).then(item => {
            projectPane.addItem(item, {index});
            typeof item.setCursorBufferPosition === 'function' &&
              item.setCursorBufferPosition(cursorPosition);
          })
        ),
        Promise.resolve()
      ).then(() => {
        projectPane.activateItemAtIndex(serializedData.activeItemIndex);
        // if there is no item in projectPane, focus tree-view;
        if (projectPane.getItems().length == 0) {
          atom.commands.dispatch(atom.views.getView(atom.workspace.element),
            'tree-view:toggle-focus');
        }
      });
    }
    catch (err) { console.error(err) }
  });
}
