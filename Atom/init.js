// manage projects

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
