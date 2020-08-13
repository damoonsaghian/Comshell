const { Disposable, CompositeDisposable } = require('event-kit');
const listen = require('./delegated-listener');
const { debounce } = require('underscore-plus');

// Handles low-level events related to the `window`.
module.exports = class WindowEventHandler {
  constructor({ atomEnvironment }) {
    this.handleDocumentKeyEvent = this.handleDocumentKeyEvent.bind(this);
    this.handleFocusNext = this.handleFocusNext.bind(this);
    this.handleFocusPrevious = this.handleFocusPrevious.bind(this);
    this.handleWindowBlur = this.handleWindowBlur.bind(this);
    this.handleEnterFullScreen = this.handleEnterFullScreen.bind(this);
    this.handleLeaveFullScreen = this.handleLeaveFullScreen.bind(this);
    this.handleWindowBeforeunload = this.handleWindowBeforeunload.bind(this);
    this.handleWindowToggleFullScreen = this.handleWindowToggleFullScreen.bind(
      this
    );
    this.handleWindowClose = this.handleWindowClose.bind(this);
    this.handleWindowToggleDevTools = this.handleWindowToggleDevTools.bind(
      this
    );
    this.handleLinkClick = this.handleLinkClick.bind(this);
    this.handleDocumentContextmenu = this.handleDocumentContextmenu.bind(this);
    this.atomEnvironment = atomEnvironment;
    this.subscriptions = new CompositeDisposable();
  }

  initialize(window, document) {
    this.window = window;
    this.document = document;
    this.subscriptions.add(
      this.atomEnvironment.commands.add(this.window, {
        'window:toggle-full-screen': this.handleWindowToggleFullScreen,
        'window:close': this.handleWindowClose,
        'window:toggle-dev-tools': this.handleWindowToggleDevTools
      })
    );

    this.subscriptions.add(
      this.atomEnvironment.commands.add(this.document, {
        'core:focus-next': this.handleFocusNext,
        'core:focus-previous': this.handleFocusPrevious
      })
    );

    this.addEventListener(
      this.window,
      'beforeunload',
      this.handleWindowBeforeunload
    );
    this.addEventListener(this.window, 'focus', this.handleWindowFocus);
    this.addEventListener(this.window, 'blur', this.handleWindowBlur);

    this.addEventListener(this.document, 'keyup', this.handleDocumentKeyEvent);
    this.addEventListener(
      this.document,
      'keydown',
      this.handleDocumentKeyEvent
    );
    this.addEventListener(this.document, 'drop', this.handleDocumentDrop);
    this.addEventListener(
      this.document,
      'dragover',
      this.handleDocumentDragover
    );
    this.addEventListener(
      this.document,
      'contextmenu',
      this.handleDocumentContextmenu
    );
    this.subscriptions.add(
      listen(this.document, 'click', 'a', this.handleLinkClick)
    );
    this.subscriptions.add(
      listen(this.document, 'submit', 'form', this.handleFormSubmit)
    );
  }

  unsubscribe() {
    this.subscriptions.dispose();
  }

  on(target, eventName, handler) {
    target.on(eventName, handler);
    this.subscriptions.add(
      new Disposable(function() {
        target.removeListener(eventName, handler);
      })
    );
  }

  addEventListener(target, eventName, handler) {
    target.addEventListener(eventName, handler);
    this.subscriptions.add(
      new Disposable(function() {
        target.removeEventListener(eventName, handler);
      })
    );
  }

  handleDocumentKeyEvent(event) {
    this.atomEnvironment.keymaps.handleKeyboardEvent(event);
    event.stopImmediatePropagation();
  }

  handleDrop(event) {
    event.preventDefault();
    event.stopPropagation();
  }

  handleDragover(event) {
    event.preventDefault();
    event.stopPropagation();
    event.dataTransfer.dropEffect = 'none';
  }

  eachTabIndexedElement(callback) {
    for (let element of this.document.querySelectorAll('[tabindex]')) {
      if (element.disabled) {
        continue;
      }
      if (!(element.tabIndex >= 0)) {
        continue;
      }
      callback(element, element.tabIndex);
    }
  }

  handleFocusNext() {
    const focusedTabIndex =
      this.document.activeElement.tabIndex != null
        ? this.document.activeElement.tabIndex
        : -Infinity;

    let nextElement = null;
    let nextTabIndex = Infinity;
    let lowestElement = null;
    let lowestTabIndex = Infinity;
    this.eachTabIndexedElement(function(element, tabIndex) {
      if (tabIndex < lowestTabIndex) {
        lowestTabIndex = tabIndex;
        lowestElement = element;
      }

      if (focusedTabIndex < tabIndex && tabIndex < nextTabIndex) {
        nextTabIndex = tabIndex;
        nextElement = element;
      }
    });

    if (nextElement != null) {
      nextElement.focus();
    } else if (lowestElement != null) {
      lowestElement.focus();
    }
  }

  handleFocusPrevious() {
    const focusedTabIndex =
      this.document.activeElement.tabIndex != null
        ? this.document.activeElement.tabIndex
        : Infinity;

    let previousElement = null;
    let previousTabIndex = -Infinity;
    let highestElement = null;
    let highestTabIndex = -Infinity;
    this.eachTabIndexedElement(function(element, tabIndex) {
      if (tabIndex > highestTabIndex) {
        highestTabIndex = tabIndex;
        highestElement = element;
      }

      if (focusedTabIndex > tabIndex && tabIndex > previousTabIndex) {
        previousTabIndex = tabIndex;
        previousElement = element;
      }
    });

    if (previousElement != null) {
      previousElement.focus();
    } else if (highestElement != null) {
      highestElement.focus();
    }
  }

  handleWindowFocus() {
    this.document.body.classList.remove('is-blurred');
  }

  handleWindowBlur() {
    this.document.body.classList.add('is-blurred');
  }

  handleWindowBeforeunload(event) {
    this.atomEnvironment.hide();
    this.atomEnvironment.destroy();
  }

  handleWindowToggleFullScreen() {
    this.atomEnvironment.toggleFullScreen();
  }

  handleWindowClose() {
    this.atomEnvironment.close();
  }

  handleWindowToggleDevTools() {
    this.atomEnvironment.toggleDevTools();
  }

  handleLinkClick(event) {
    event.preventDefault();
    const uri = event.currentTarget && event.currentTarget.getAttribute('href');
    if (uri && uri[0] !== '#') {
      if (/^https?:\/\//.test(uri)) {
        nw.Shell.openExternal(uri);
      } else if (uri.startsWith('atom://')) {
        this.atomEnvironment.uriHandlerRegistry.handleURI(uri);
      }
    }
  }

  handleFormSubmit(event) {
    // Prevent form submits from changing the current window's URL
    event.preventDefault();
  }

  handleDocumentContextmenu(event) {
    event.preventDefault();
    this.atomEnvironment.contextMenu.showForEvent(event);
  }
};
