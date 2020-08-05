'use strict';

const focusTrap = require('focus-trap');
const { Emitter, CompositeDisposable } = require('event-kit');

class PanelContainerElement extends HTMLElement {
  createdCallback() {
    this.subscriptions = new CompositeDisposable();
  }

  attachedCallback() {
    if (this.model.dock) {
      this.model.dock.elementAttached();
    }
  }

  initialize(model, viewRegistry) {
    this.model = model;
    this.viewRegistry = viewRegistry;

    this.subscriptions.add(
      this.model.onDidAddPanel(this.panelAdded.bind(this))
    );
    this.subscriptions.add(this.model.onDidDestroy(this.destroyed.bind(this)));
    this.classList.add(this.model.getLocation());

    // Add the dock.
    if (this.model.dock != null) {
      this.appendChild(this.model.dock.getElement());
    }

    return this;
  }

  getModel() {
    return this.model;
  }

  panelAdded({ panel, index }) {
    const panelElement = panel.getElement();
    panelElement.classList.add(this.model.getLocation());
    if (this.model.isModal()) {
      panelElement.classList.add('overlay', 'from-top');
    } else {
      panelElement.classList.add(
        'tool-panel',
        `panel-${this.model.getLocation()}`
      );
    }

    if (index >= this.childNodes.length) {
      this.appendChild(panelElement);
    } else {
      const referenceItem = this.childNodes[index];
      this.insertBefore(panelElement, referenceItem);
    }

    if (this.model.isModal()) {
      this.hideAllPanelsExcept(panel);
      this.subscriptions.add(
        panel.onDidChangeVisible(visible => {
          if (visible) {
            this.hideAllPanelsExcept(panel);
          }
        })
      );

      if (panel.autoFocus) {
        const focusOptions = {
          // focus-trap will attempt to give focus to the first tabbable element
          // on activation. If there aren't any tabbable elements,
          // give focus to the panel element itself
          fallbackFocus: panelElement,
          // closing is handled by core Atom commands and this already deactivates
          // on visibility changes
          escapeDeactivates: false
        };

        if (panel.autoFocus !== true) {
          focusOptions.initialFocus = panel.autoFocus;
        }
        const modalFocusTrap = focusTrap(panelElement, focusOptions);

        this.subscriptions.add(
          panel.onDidChangeVisible(visible => {
            if (visible) {
              modalFocusTrap.activate();
            } else {
              modalFocusTrap.deactivate();
            }
          })
        );
      }
    }
  }

  destroyed() {
    this.subscriptions.dispose();
    if (this.parentNode != null) {
      this.parentNode.removeChild(this);
    }
  }

  hideAllPanelsExcept(excludedPanel) {
    for (let panel of this.model.getPanels()) {
      if (panel !== excludedPanel) {
        panel.hide();
      }
    }
  }
}

window.customElements.define('atom-panel-container', PanelContainerElement);

module.exports = class PanelContainer {
  constructor({ location, dock, viewRegistry } = {}) {
    this.location = location;
    this.emitter = new Emitter();
    this.subscriptions = new CompositeDisposable();
    this.panels = [];
    this.dock = dock;
    this.viewRegistry = viewRegistry;
  }

  destroy() {
    for (let panel of this.getPanels()) {
      panel.destroy();
    }
    this.subscriptions.dispose();
    this.emitter.emit('did-destroy', this);
    this.emitter.dispose();
  }

  getElement() {
    if (!this.element) {
      this.element = new PanelContainerElement().initialize(
        this,
        this.viewRegistry
      );
    }
    return this.element;
  }

  /*
  Section: Event Subscription
  */

  onDidAddPanel(callback) {
    return this.emitter.on('did-add-panel', callback);
  }

  onDidRemovePanel(callback) {
    return this.emitter.on('did-remove-panel', callback);
  }

  onDidDestroy(callback) {
    return this.emitter.once('did-destroy', callback);
  }

  /*
  Section: Panels
  */

  getLocation() {
    return this.location;
  }

  isModal() {
    return this.location === 'modal';
  }

  getPanels() {
    return this.panels.slice();
  }

  addPanel(panel) {
    this.subscriptions.add(panel.onDidDestroy(this.panelDestroyed.bind(this)));

    const index = this.getPanelIndex(panel);
    if (index === this.panels.length) {
      this.panels.push(panel);
    } else {
      this.panels.splice(index, 0, panel);
    }

    this.emitter.emit('did-add-panel', { panel, index });
    return panel;
  }

  panelForItem(item) {
    for (let panel of this.panels) {
      if (panel.getItem() === item) {
        return panel;
      }
    }
    return null;
  }

  panelDestroyed(panel) {
    const index = this.panels.indexOf(panel);
    if (index > -1) {
      this.panels.splice(index, 1);
      this.emitter.emit('did-remove-panel', { panel, index });
    }
  }

  getPanelIndex(panel) {
    const priority = panel.getPriority();
    if (['bottom', 'right'].includes(this.location)) {
      for (let i = this.panels.length - 1; i >= 0; i--) {
        const p = this.panels[i];
        if (priority < p.getPriority()) {
          return i + 1;
        }
      }
      return 0;
    } else {
      for (let i = 0; i < this.panels.length; i++) {
        const p = this.panels[i];
        if (priority < p.getPriority()) {
          return i;
        }
      }
      return this.panels.length;
    }
  }
};
