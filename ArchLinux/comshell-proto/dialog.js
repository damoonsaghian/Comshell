const { Emitter, Disposable, CompositeDisposable } = require('event-kit');
const TextEditor = require('./editor/text-editor');
const { Point, Range } = require('text-buffer');

module.exports = class Dialog {
  constructor({ prompt, defaultAnswer, select, iconClass } = {}) {
    this.emitter = new Emitter();
    this.disposables = new CompositeDisposable();

    this.element = document.createElement('div');

    this.promptText = document.createElement('label');
    this.promptText.classList.add('icon');
    if (iconClass) this.promptText.classList.add(iconClass);
    this.promptText.textContent = prompt;
    this.element.appendChild(this.promptText);

    this.miniEditor = new TextEditor({ mini: true });
    blurHandler = () => {
      if (document.hasFocus()) this.close();
    }
    this.miniEditor.element.addEventListener('blur', blurHandler);
    this.disposables.add(new Disposable(() =>
      this.miniEditor.element.removeEventListener('blur', blurHandler))
    );
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

    if (typeof defaultAnswer === 'string') {
      this.miniEditor.setText(defaultAnswer);
      if (select) this.miniEditor.setSelectedBufferRange(Range(
        Point(0, 0),
        Point(0, defaultAnswer.length)
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

  cancel() {
    this.close();
    atom.workspace.getCenter().activate();
    this.onClose();
  }

  showError(message='') {
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
