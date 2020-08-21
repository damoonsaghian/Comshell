const { Emitter, Disposable, CompositeDisposable } = require('event-kit');
const TextEditor = require('../editor/text-editor');
const { Point, Range } = require('text-buffer');

const fs = require('fs-plus');
const path = require('path');
const { getFullExtension, repoForPath } = require('./helpers');

class Dialog {
  constructor({ initialPath, select, iconClass, prompt } = {}) {
    this.emitter = new Emitter();
    this.disposables = new CompositeDisposable();

    this.element = document.createElement('div');
    this.element.classList.add('tree-view-dialog');

    this.promptText = document.createElement('label');
    this.promptText.classList.add('icon');
    if (iconClass) this.promptText.classList.add(iconClass);
    this.promptText.textContent = prompt;
    this.element.appendChild(this.promptText);

    this.miniEditor = new TextEditor({ mini: true });
    const blurHandler = () => {
      if (document.hasFocus()) this.close();
    }
    this.miniEditor.element.addEventListener('blur', blurHandler);
    this.disposables.add(new Disposable(
      () => this.miniEditor.element.removeEventListener('blur', blurHandler)
    ));
    this.disposables.add(this.miniEditor.onDidChange(
      () => this.showError()
    ));
    this.element.appendChild(this.miniEditor.element);

    this.errorMessage = document.createElement('div');
    this.errorMessage.classList.add('error-message');
    this.element.appendChild(this.errorMessage);

    atom.commands.add(this.element, {
      'core:confirm': () => this.onConfirm(this.miniEditor.getText()),
      'core:cancel': () => this.cancel()
    });

    this.miniEditor.setText(initialPath);

    if (select) {
      const extension = getFullExtension(initialPath);
      const baseName = path.basename(initialPath);
      const selectionStart = initialPath.length - baseName.length;
      let selectionEnd;
      if (baseName === extension) {
        selectionEnd = initialPath.length;
      } else {
        selectionEnd = initialPath.length - extension.length;
      }
      this.miniEditor.setSelectedBufferRange(Range(
        Point(0, selectionStart),
        Point(0, selectionEnd)
      ));
    }
  }

  attach() {
    this.panel = atom.workspace.addModalPanel({ item: this });
    this.miniEditor.element.focus();
    this.miniEditor.scrollToCursorPosition();
  }

  close() {
    const panel = this.panel;
    this.panel = null;
    panel?.destroy();
    this.emitter.dispose();
    this.disposables.dispose();
    this.miniEditor.destroy();
    const activePane = atom.workspace.getCenter().getActivePane();
    if (!activePane.isDestroyed()) activePane.activate();
  }

  cancel(){
    this.close();
    document.querySelector('.tree-view')?.focus();
  }

  showError(message = '') {
    this.errorMessage.textContent = message;
    if (message) {
      this.element.classList.add('error');
      window.setTimeout(
        () => this.element.classList.remove('error'),
        300
      );
    }
  }
}

class AddDialog extends Dialog {
  constructor(initialPath, isCreatingFile) {
    this.isCreatingFile = isCreatingFile;

    let directoryPath;
    if (fs.isFileSync(initialPath)) {
      directoryPath = path.dirname(initialPath);
    } else {
      directoryPath = initialPath;
    }

    let relativeDirectoryPath = directoryPath;
    [rootProjectPath, relativeDirectoryPath] = atom.project.relativizePath(directoryPath);
    this.rootProjectPath = rootProjectPath;
    if (relativeDirectoryPath.length > 0)
      relativeDirectoryPath += path.sep;

    super({
      prompt: "Enter the path for the new " + (isCreatingFile ? "file." : "folder."),
      initialPath: relativeDirectoryPath,
      select: false,
      iconClass: isCreatingFile ? 'icon-file-add' : 'icon-file-directory-create'
    });
  }

  onDidCreateFile(callback) {
    this.emitter.on('did-create-file', callback);
  }

  onDidCreateDirectory(callback) {
    this.emitter.on('did-create-directory', callback);
  }

  onConfirm(newPath) {
    let newPath = newPath.replace(/\s+$/, ''); // remove trailing whitespace
    const endsWithDirectorySeparator = newPath[newPath.length - 1] === path.sep;
    if (!path.isAbsolute(newPath)) {
      if (this.rootProjectPath == null) {
        this.showError("You must open a directory to create a file with a relative path");
        return;
      }
      newPath = path.join(this.rootProjectPath, newPath);
    }

    if (!newPath) return;

    try {
      if (fs.existsSync(newPath)) {
        this.showError(`'${newPath}' already exists.`);
      } else if (this.isCreatingFile) {
        if (endsWithDirectorySeparator) {
          this.showError(`File names must not end with a '${path.sep}' character.`);
        } else {
          fs.writeFileSync(newPath, '');
          repoForPath(newPath)?.getPathStatus(newPath);
          this.emitter.emit('did-create-file', newPath);
          this.close();
        }
      } else {
        fs.makeTreeSync(newPath);
        this.emitter.emit('did-create-directory', newPath);
        this.cancel();
      }
    } catch (error) {
      this.showError(`${error.message}.`);
    }
  }
}

class CopyDialog extends Dialog {
  constructor(initialPath, {onCopy}) {
    this.initialPath = initialPath;
    this.onCopy = onCopy;
    super({
      prompt: 'Enter the new path for the duplicate.',
      initialPath: atom.project.relativize(this.initialPath),
      select: true,
      iconClass: 'icon-arrow-right'
    });
  }

  onConfirm(newPath) {
    let newPath = newPath.replace(/\s+$/, ''); // remove trailing whitespace
    if (!path.isAbsolute(newPath)) {
      [rootPath] = atom.project.relativizePath(this.initialPath);
      newPath = path.join(rootPath, newPath);
      if (!newPath) return;
    }

    if (this.initialPath === newPath) {
      this.close();
      return;
    }

    if (!this.isNewPathValid(newPath)) {
      this.showError(`'${newPath}' already exists.`);
      return;
    }

    let activeEditor = atom.workspace.getActiveTextEditor();
    if (activeEditor?.getPath() !== this.initialPath)
      activeEditor = null;

    try {
      if (fs.isDirectorySync(this.initialPath)) {
        fs.copySync(this.initialPath, newPath);
        if (typeof this.onCopy === "function")
          this.onCopy({
            initialPath: this.initialPath,
            newPath: newPath
          });
      } else {
        fs.copy(this.initialPath, newPath, () => {
          if (typeof this.onCopy === "function")
            this.onCopy({
              initialPath: this.initialPath,
              newPath: newPath
            });
          atom.workspace.open(newPath, {
            activatePane: true,
            initialLine: activeEditor?.getLastCursor().getBufferRow(),
            initialColumn: activeEditor?.getLastCursor().getBufferColumn()
          });
        });
      }

      const repo = repoForPath(newPath);
      if (repo) {
        repo.getPathStatus(this.initialPath);
        repo.getPathStatus(newPath);
      }
      this.close();
    } catch (error) {
      this.showError(`${error.message}.`);
    }
  }

  isNewPathValid(newPath) {
    try {
      const oldStat = fs.statSync(this.initialPath)
      const newStat = fs.statSync(newPath)

      // New path exists so check if it points to the same file as the initial
      // path to see if the case of the file name is being changed on a on a
      // case insensitive filesystem.
      return this.initialPath.toLowerCase() === newPath.toLowerCase() &&
        oldStat.dev === newStat.dev &&
        oldStat.ino === newStat.ino
    } catch {
      return true; // new path does not exist so it is valid
    }
  }
}

class MoveDialog extends Dialog {
  constructor(initialPath, { willMove, onMove, onMoveFailed}) {
    this.initialPath = initialPath;
    this.willMove = willMove;
    this.onMove = onMove;
    this.onMoveFailed = onMoveFailed;

    let prompt;
    if (fs.isDirectorySync(this.initialPath) {
      prompt = 'Enter the new path for the directory.';
    } else {
      prompt = 'Enter the new path for the file.';
    }

    super({
      prompt: prompt,
      initialPath: atom.project.relativize(this.initialPath),
      select: true,
      iconClass: 'icon-arrow-right'
    });
  }

  onConfirm(newPath) {
    let newPath = newPath.replace(/\s+$/, ''); // remove trailing whitespace
    if (!path.isAbsolute(newPath)) {
      [rootPath] = atom.project.relativizePath(this.initialPath);
      newPath = path.join(rootPath, newPath);
      if (!newPath) return;
    }

    if (this.initialPath === newPath) {
      this.close();
      return;
    }

    if (!this.isNewPathValid(newPath)) {
      this.showError(`'${newPath}' already exists.`);
      return;
    }

    const directoryPath = path.dirname(newPath);
    try {
      if (typeof this.willMove === "function")
        this.willMove({
          initialPath: this.initialPath,
          newPath: newPath
        });
      if (!fs.existsSync(directoryPath))
        fs.makeTreeSync(directoryPath);

      fs.moveSync(this.initialPath, newPath);
      if (typeof this.onMove === "function")
        this.onMove({
          initialPath: this.initialPath,
          newPath: newPath
        });

      const repo = repoForPath(newPath);
      if (repo) {
        repo.getPathStatus(this.initialPath);
        repo.getPathStatus(newPath);
      }
      this.close();
    } catch (error) {
      this.showError(`${error.message}.`);
      if (typeof this.onMoveFailed === "function")
        this.onMoveFailed({
          initialPath: this.initialPath,
          newPath: newPath
        });
    }
  }

  isNewPathValid(newPath) {
    try {
      const oldStat = fs.statSync(this.initialPath);
      const newStat = fs.statSync(newPath);

      // New path exists so check if it points to the same file as the initial
      // path to see if the case of the file name is being changed on a on a
      // case insensitive filesystem.
      return this.initialPath.toLowerCase() === newPath.toLowerCase() &&
        oldStat.dev === newStat.dev &&
        oldStat.ino === newStat.ino
    } catch {
      return true; // new path does not exist so it is valid
    }
  }
}

module.exports = { AddDialog, CopyDialog, MoveDialog };
