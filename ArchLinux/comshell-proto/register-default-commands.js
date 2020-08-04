const nw = require('nw');

module.exports = ({ commandRegistry, config, notificationManager, project, clipboard }) => {
  commandRegistry.add(
    'atom-workspace',
    {
      'pane:show-next-item': function () { this.getModel().getActivePane().activateNextItem() },
      'pane:show-previous-item': function () {
        this.getModel().getActivePane().activatePreviousItem();
      },
      'application:quit': () => nw.Window.get().close(),
      'window:focus-next-pane': function () { this.getModel().activateNextPane() },
      'window:focus-previous-pane': function () { this.getModel().activatePreviousPane() },
      'window:focus-pane-above': function () { this.focusPaneViewAbove() },
      'window:focus-pane-below': function () { this.focusPaneViewBelow() },
      'window:focus-pane-on-left': function () { this.focusPaneViewOnLeft() },
      'window:focus-pane-on-right': function () { this.focusPaneViewOnRight() },
      'window:save-all': function () { this.getModel().saveAll() },
      'core:close': function () { this.getModel().closeActivePaneItemOrEmptyPaneOrWindow() },
      'core:save': function () { this.getModel().saveActivePaneItem() }
    },
    false
  );

  commandRegistry.add(
    'atom-text-editor',
    stopEventPropagation({
      'core:move-left': function () { this.moveLeft() },
      'core:move-right': function () { this.moveRight() },
      'core:select-left': function () { this.selectLeft() },
      'core:select-right': function () { this.selectRight() },
      'core:select-up': function () { this.selectUp() },
      'core:select-down': function () { this.selectDown() },
      'core:select-all': function () { this.selectAll() },
      'editor:select-word': function () { this.selectWordsContainingCursors() },
      'editor:consolidate-selections': function(event) {
        if (!this.consolidateSelections()) event.abortKeyBinding();
      },
      'editor:move-to-beginning-of-next-paragraph': function () {
        this.moveToBeginningOfNextParagraph();
      },
      'editor:move-to-beginning-of-previous-paragraph': function () {
        this.moveToBeginningOfPreviousParagraph();
      },
      'editor:move-to-beginning-of-screen-line': function () { this.moveToBeginningOfScreenLine() },
      'editor:move-to-beginning-of-line': function () { this.moveToBeginningOfLine() },
      'editor:move-to-end-of-screen-line': function () { this.moveToEndOfScreenLine() },
      'editor:move-to-end-of-line': function () { this.moveToEndOfLine() },
      'editor:move-to-first-character-of-line': function () { this.moveToFirstCharacterOfLine() },
      'editor:move-to-beginning-of-word': function () { this.moveToBeginningOfWord() },
      'editor:move-to-end-of-word': function () { this.moveToEndOfWord() },
      'editor:move-to-beginning-of-next-word': function () { this.moveToBeginningOfNextWord() },
      'editor:move-to-previous-word-boundary': function () { this.moveToPreviousWordBoundary() },
      'editor:move-to-next-word-boundary': function () { this.moveToNextWordBoundary() },
      'editor:move-to-previous-subword-boundary': function () {
        this.moveToPreviousSubwordBoundary();
      },
      'editor:move-to-next-subword-boundary': function () { this.moveToNextSubwordBoundary() },
      'editor:select-to-beginning-of-next-paragraph': function () {
        this.selectToBeginningOfNextParagraph();
      },
      'editor:select-to-beginning-of-previous-paragraph': function () {
        this.selectToBeginningOfPreviousParagraph();
      },
      'editor:select-to-end-of-line': function () { this.selectToEndOfLine() },
      'editor:select-to-beginning-of-line': function () { this.selectToBeginningOfLine() },
      'editor:select-to-end-of-word': function () { this.selectToEndOfWord() },
      'editor:select-to-beginning-of-word': function () { this.selectToBeginningOfWord() },
      'editor:select-to-beginning-of-next-word': function () { this.selectToBeginningOfNextWord() },
      'editor:select-to-next-word-boundary': function () { this.selectToNextWordBoundary() },
      'editor:select-to-previous-word-boundary': function () {
        this.selectToPreviousWordBoundary();
      },
      'editor:select-to-next-subword-boundary': function () { this.selectToNextSubwordBoundary() },
      'editor:select-to-previous-subword-boundary': function () {
        this.selectToPreviousSubwordBoundary();
      },
      'editor:select-to-first-character-of-line': function () {
        this.selectToFirstCharacterOfLine();
      },
      'editor:select-line': function () { this.selectLinesContainingCursors() },
      'editor:select-larger-syntax-node': function () { this.selectLargerSyntaxNode() },
      'editor:select-smaller-syntax-node': function () { this.selectSmallerSyntaxNode() }
    }),
    false
  );

  commandRegistry.add(
    'atom-text-editor:not([readonly])',
    stopEventPropagation({
      'core:undo': function () { this.undo() },
      'core:redo': function () { this.redo() }
    }),
    false
  );

  commandRegistry.add(
    'atom-text-editor',
    stopEventPropagationAndGroupUndo(
      config,
      {
        'core:copy': function () { this.copySelectedText() },
        'editor:copy-selection': function () { this.copyOnlySelectedText() }
      }
    ),
    false
  );

  commandRegistry.add(
    'atom-text-editor:not([readonly])',
    stopEventPropagationAndGroupUndo(
      config,
      {
        'core:backspace': function () { this.backspace() },
        'core:delete': function () { this.delete() },
        'core:cut': function () { this.cutSelectedText() },
        'core:paste': function () { this.pasteText() },
        'editor:paste-without-reformatting': function () {
          this.pasteText({
            normalizeLineEndings: false,
            autoIndent: false,
            preserveTrailingLineIndentation: true
          })
        },
        'editor:delete-to-previous-word-boundary': function () {
          this.deleteToPreviousWordBoundary();
        },
        'editor:delete-to-next-word-boundary': function () { this.deleteToNextWordBoundary() },
        'editor:delete-to-beginning-of-word': function () { this.deleteToBeginningOfWord() },
        'editor:delete-to-beginning-of-line': function () { this.deleteToBeginningOfLine() },
        'editor:delete-to-end-of-line': function () { this.deleteToEndOfLine() },
        'editor:delete-to-end-of-word': function () { this.deleteToEndOfWord() },
        'editor:delete-to-beginning-of-subword': function () {
          this.deleteToBeginningOfSubword();
        },
        'editor:delete-to-end-of-subword': function () { this.deleteToEndOfSubword() },
        'editor:delete-line': function () { this.deleteLine() },
        'editor:cut-to-end-of-line': function () { this.cutToEndOfLine() },
        'editor:cut-to-end-of-buffer-line': function () { this.cutToEndOfBufferLine() },
        'editor:transpose': function () { this.transpose() },
        'editor:upper-case': function () { this.upperCase() },
        'editor:lower-case': function () { this.lowerCase() }
      }
    ),
    false
  );

  commandRegistry.add(
    'atom-text-editor:not([mini])',
    stopEventPropagation({
      'core:move-up': function () { this.moveUp() },
      'core:move-down': function () { this.moveDown() },
      'core:move-to-top': function () { this.moveToTop() },
      'core:move-to-bottom': function () { this.moveToBottom() },
      'core:page-up': function () { this.pageUp() },
      'core:page-down': function () { this.pageDown() },
      'core:select-to-top': function () { this.selectToTop() },
      'core:select-to-bottom': function () { this.selectToBottom() },
      'core:select-page-up': function () { this.selectPageUp() },
      'core:select-page-down': function () { this.selectPageDown() },
      'editor:add-selection-below': function () { this.addSelectionBelow() },
      'editor:add-selection-above': function () { this.addSelectionAbove() },
      'editor:split-selections-into-lines': function () { this.splitSelectionsIntoLines() },
      'editor:toggle-soft-tabs': function () { this.toggleSoftTabs() },
      'editor:toggle-soft-wrap': function () { this.toggleSoftWrapped() },
      'editor:fold-all': function () { this.foldAll() },
      'editor:unfold-all': function () { this.unfoldAll() },
      'editor:fold-selection': function () { this.foldSelectedLines() },
      'editor:copy-path': () => copyPathToClipboard(this, project, clipboard, false),
      'editor:copy-project-path': () =>
        copyPathToClipboard(this, project, clipboard, true),
      'editor:toggle-indent-guide': () =>
        config.set('editor.showIndentGuide', !config.get('editor.showIndentGuide')),
      'editor:toggle-line-numbers': () =>
        config.set('editor.showLineNumbers', !config.get('editor.showLineNumbers')),
      'editor:scroll-to-cursor': function () { this.scrollToCursorPosition() }
    }),
    false
  );

  commandRegistry.add(
    'atom-text-editor:not([mini]):not([readonly])',
    stopEventPropagationAndGroupUndo(
      config,
      {
        'editor:indent': function () { this.indent() },
        'editor:auto-indent': function () { this.autoIndentSelectedRows() },
        'editor:indent-selected-rows': function () { this.indentSelectedRows() },
        'editor:outdent-selected-rows': function () { this.outdentSelectedRows() },
        'editor:newline': function () { this.insertNewline() },
        'editor:newline-below': function () { this.insertNewlineBelow() },
        'editor:newline-above': function () { this.insertNewlineAbove() },
        'editor:toggle-line-comments': function () { this.toggleLineCommentsInSelection() },
        'editor:checkout-head-revision': function () {
          atom.workspace.checkoutHeadRevision(this);
        },
        'editor:move-line-up': function () { this.moveLineUp() },
        'editor:move-line-down': function () { this.moveLineDown() },
        'editor:move-selection-left': function () { this.moveSelectionLeft() },
        'editor:move-selection-right': function () { this.moveSelectionRight() },
        'editor:duplicate-lines': function () { this.duplicateLines() },
        'editor:join-lines': function () { this.joinLines() }
      }
    ),
    false
  );
};

function stopEventPropagation(commandListeners) {
  let newCommandListeners = {};
  for (const commandName in commandListeners) {
    const commandListener = commandListeners[commandName];
    newCommandListeners[commandName] = function(event) {
      event.stopPropagation();
      commandListener.call(this.getModel(), event);
    }
  }
  return newCommandListeners;
}

function stopEventPropagationAndGroupUndo(config, commandListeners) {
  let newCommandListeners = {};
  for (commandName in commandListeners) {
    const commandListener = commandListeners[commandName];
    newCommandListeners[commandName] = function(event) {
      event.stopPropagation();
      const model = this.getModel();
      model.transact(
        model.getUndoGroupingInterval(),
        () => commandListener.call(model, event)
      )
    };
  }
  return newCommandListeners;
}

function copyPathToClipboard(editor, project, clipboard, relative) {
  let filePath = editor.getPath();
  if (filePath) {
    if (relative) filePath = project.relativize(filePath);
    clipboard.write(filePath);
  }
}
