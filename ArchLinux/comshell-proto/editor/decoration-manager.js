const { Emitter } = require('event-kit');

// Essential: Represents a decoration that follows a {DisplayMarker}. A decoration is
// basically a visual representation of a marker. It allows you to add CSS
// classes to line numbers in the gutter, lines, and add selection-line regions
// around marked ranges of text.
//
// {Decoration} objects are not meant to be created directly, but created with
// {TextEditor::decorateMarker}. eg.
//
// ```coffee
// range = editor.getSelectedBufferRange() # any range you like
// marker = editor.markBufferRange(range)
// decoration = editor.decorateMarker(marker, {type: 'line', class: 'my-line-class'})
// ```
//
// Best practice for destroying the decoration is by destroying the {DisplayMarker}.
//
// ```coffee
// marker.destroy()
// ```
//
// You should only use {Decoration::destroy} when you still need or do not own
// the marker.
class Decoration {
  static idCounter = 0;
  static nextId() { this.idCounter++; }

  static normalizeDecorationProperties(decoration, decorationParams) {
    decorationParams.id = decoration.id;

    if (
      decorationParams.type === 'line-number' &&
      decorationParams.gutterName == null
    ) {
      decorationParams.gutterName = 'line-number';
    }

    if (decorationParams.order == null) {
      decorationParams.order = Infinity;
    }

    return decorationParams;
  };

  // Private: Check if the `decorationProperties.type` matches `type`
  //
  // * `decorationProperties` {Object} eg. `{type: 'line-number', class: 'my-new-class'}`
  // * `type` {String} type like `'line-number'`, `'line'`, etc. `type` can also
  //   be an {Array} of {String}s, where it will return true if the decoration's
  //   type matches any in the array.
  //
  // Returns {Boolean}
  // Note: 'line-number' is a special subtype of the 'gutter' type. I.e., a
  // 'line-number' is a 'gutter', but a 'gutter' is not a 'line-number'.
  static isType(decorationProperties, type) {
    // 'line-number' is a special case of 'gutter'.
    if (Array.isArray(decorationProperties.type)) {
      if (decorationProperties.type.includes(type)) {
        return true;
      }

      if (
        type === 'gutter' &&
        decorationProperties.type.includes('line-number')
      ) {
        return true;
      }

      return false;
    } else {
      if (type === 'gutter') {
        return ['gutter', 'line-number'].includes(decorationProperties.type);
      } else {
        return type === decorationProperties.type;
      }
    }
  }

  /*
  Section: Construction and Destruction
  */

  constructor(marker, decorationManager, properties) {
    this.marker = marker;
    this.decorationManager = decorationManager;
    this.emitter = new Emitter();
    this.id = Decoration.nextId();
    this.setProperties(properties);
    this.destroyed = false;
    this.markerDestroyDisposable = this.marker.onDidDestroy(() =>
      this.destroy()
    );
  }

  // Essential: Destroy this marker decoration.
  //
  // You can also destroy the marker if you own it, which will destroy this
  // decoration.
  destroy() {
    if (this.destroyed) {
      return;
    }
    this.markerDestroyDisposable.dispose();
    this.markerDestroyDisposable = null;
    this.destroyed = true;
    this.decorationManager.didDestroyMarkerDecoration(this);
    this.emitter.emit('did-destroy');
    return this.emitter.dispose();
  }

  isDestroyed() {
    return this.destroyed;
  }

  /*
  Section: Event Subscription
  */

  // Essential: When the {Decoration} is updated via {Decoration::update}.
  //
  // * `callback` {Function}
  //   * `event` {Object}
  //     * `oldProperties` {Object} the old parameters the decoration used to have
  //     * `newProperties` {Object} the new parameters the decoration now has
  //
  // Returns a {Disposable} on which `.dispose()` can be called to unsubscribe.
  onDidChangeProperties(callback) {
    return this.emitter.on('did-change-properties', callback);
  }

  // Essential: Invoke the given callback when the {Decoration} is destroyed
  //
  // * `callback` {Function}
  //
  // Returns a {Disposable} on which `.dispose()` can be called to unsubscribe.
  onDidDestroy(callback) {
    return this.emitter.once('did-destroy', callback);
  }

  /*
  Section: Decoration Details
  */

  // Essential: An id unique across all {Decoration} objects
  getId() {
    return this.id;
  }

  // Essential: Returns the marker associated with this {Decoration}
  getMarker() {
    return this.marker;
  }

  // Public: Check if this decoration is of type `type`
  //
  // * `type` {String} type like `'line-number'`, `'line'`, etc. `type` can also
  //   be an {Array} of {String}s, where it will return true if the decoration's
  //   type matches any in the array.
  //
  // Returns {Boolean}
  isType(type) {
    return Decoration.isType(this.properties, type);
  }

  /*
  Section: Properties
  */

  // Essential: Returns the {Decoration}'s properties.
  getProperties() {
    return this.properties;
  }

  // Essential: Update the marker with new Properties. Allows you to change the decoration's class.
  //
  // ## Examples
  //
  // ```coffee
  // decoration.setProperties({type: 'line-number', class: 'my-new-class'})
  // ```
  //
  // * `newProperties` {Object} eg. `{type: 'line-number', class: 'my-new-class'}`
  setProperties(newProperties) {
    if (this.destroyed) {
      return;
    }
    const oldProperties = this.properties;
    this.properties = Decoration.normalizeDecorationProperties(this, newProperties);
    if (newProperties.type != null) {
      this.decorationManager.decorationDidChangeType(this);
    }
    this.decorationManager.emitDidUpdateDecorations();
    return this.emitter.emit('did-change-properties', {
      oldProperties,
      newProperties
    });
  }

  /*
  Section: Utility
  */

  inspect() {
    return `<Decoration ${this.id}>`;
  }

  /*
  Section: Private methods
  */

  matchesPattern(decorationPattern) {
    if (decorationPattern == null) {
      return false;
    }
    for (let key in decorationPattern) {
      const value = decorationPattern[key];
      if (this.properties[key] !== value) {
        return false;
      }
    }
    return true;
  }

  flash(klass, duration) {
    if (duration == null) {
      duration = 500;
    }
    this.properties.flashRequested = true;
    this.properties.flashClass = klass;
    this.properties.flashDuration = duration;
    this.decorationManager.emitDidUpdateDecorations();
    return this.emitter.emit('did-flash');
  }
};

// Essential: Represents a decoration that applies to every marker on a given
// layer. Created via {TextEditor::decorateMarkerLayer}.
class LayerDecoration {
  static idCounter = 0;
  static nextId() { this.idCounter++; }

  constructor(markerLayer, decorationManager, properties) {
    this.markerLayer = markerLayer;
    this.decorationManager = decorationManager;
    this.properties = properties;

    this.id = LayerDecoration.nextId();
    this.destroyed = false;
    this.markerLayerDestroyedDisposable = this.markerLayer.onDidDestroy => this.destroy();
    this.overridePropertiesByMarker = null;
  }

  // Essential: Destroys the decoration.
  destroy() {
    if (this.destroyed) { return; }
    this.markerLayerDestroyedDisposable.dispose();
    this.markerLayerDestroyedDisposable = null;
    this.destroyed = true;
    this.decorationManager.didDestroyLayerDecoration(this);
  }

  // Essential: Determine whether this decoration is destroyed.
  //
  // Returns a {Boolean}.
  isDestroyed() { return this.destroyed; }

  getId() { return this.id; }

  getMarkerLayer() { return this.markerLayer; }

  // Essential: Get this decoration's properties.
  //
  // Returns an {Object}.
  getProperties() { return this.properties; }

  // Essential: Set this decoration's properties.
  //
  // * `newProperties` See {TextEditor::decorateMarker} for more information on
  //   the properties. The `type` of `gutter` and `overlay` are not supported on
  //   layer decorations.
  setProperties(newProperties) {
    if (this.destroyed) { return; }
    this.properties = newProperties;
    this.decorationManager.emitDidUpdateDecorations();
  }

  // Essential: Override the decoration properties for a specific marker.
  //
  // * `marker` The {DisplayMarker} or {Marker} for which to override
  //   properties.
  // * `properties` An {Object} containing properties to apply to this marker.
  //   Pass `null` to clear the override.
  setPropertiesForMarker(marker, properties) {
    if (this.destroyed) { return; }
    if (this.overridePropertiesByMarker == null) {
      this.overridePropertiesByMarker = new Map();
    }
    marker = this.markerLayer.getMarker(marker.id);
    if (properties != null) {
      this.overridePropertiesByMarker.set(marker, properties);
    } else {
      this.overridePropertiesByMarker.delete(marker);
    }
    this.decorationManager.emitDidUpdateDecorations();
  }

  getPropertiesForMarker(marker) {
    this.overridePropertiesByMarker?.get(marker);
  }
}

module.exports = class DecorationManager {
  constructor(editor) {
    this.editor = editor;
    this.displayLayer = this.editor.displayLayer;

    this.emitter = new Emitter();
    this.decorationCountsByLayer = new Map();
    this.markerDecorationCountsByLayer = new Map();
    this.decorationsByMarker = new Map();
    this.layerDecorationsByMarkerLayer = new Map();
    this.overlayDecorations = new Set();
    this.layerUpdateDisposablesByLayer = new WeakMap();
  }

  observeDecorations(callback) {
    const decorations = this.getDecorations();
    for (let i = 0; i < decorations.length; i++) {
      callback(decorations[i]);
    }
    return this.onDidAddDecoration(callback);
  }

  onDidAddDecoration(callback) {
    return this.emitter.on('did-add-decoration', callback);
  }

  onDidRemoveDecoration(callback) {
    return this.emitter.on('did-remove-decoration', callback);
  }

  onDidUpdateDecorations(callback) {
    return this.emitter.on('did-update-decorations', callback);
  }

  getDecorations(propertyFilter) {
    let allDecorations = [];

    this.decorationsByMarker.forEach(decorations => {
      decorations.forEach(decoration => allDecorations.push(decoration));
    });
    if (propertyFilter != null) {
      allDecorations = allDecorations.filter(function(decoration) {
        for (let key in propertyFilter) {
          const value = propertyFilter[key];
          if (decoration.properties[key] !== value) return false;
        }
        return true;
      });
    }
    return allDecorations;
  }

  getLineDecorations(propertyFilter) {
    return this.getDecorations(propertyFilter).filter(decoration =>
      decoration.isType('line')
    );
  }

  getLineNumberDecorations(propertyFilter) {
    return this.getDecorations(propertyFilter).filter(decoration =>
      decoration.isType('line-number')
    );
  }

  getHighlightDecorations(propertyFilter) {
    return this.getDecorations(propertyFilter).filter(decoration =>
      decoration.isType('highlight')
    );
  }

  getOverlayDecorations(propertyFilter) {
    const result = [];
    result.push(...Array.from(this.overlayDecorations));
    if (propertyFilter != null) {
      return result.filter(function(decoration) {
        for (let key in propertyFilter) {
          const value = propertyFilter[key];
          if (decoration.properties[key] !== value) {
            return false;
          }
        }
        return true;
      });
    } else {
      return result;
    }
  }

  decorationPropertiesByMarkerForScreenRowRange(startScreenRow, endScreenRow) {
    const decorationPropertiesByMarker = new Map();

    this.decorationCountsByLayer.forEach((count, markerLayer) => {
      const markers = markerLayer.findMarkers({
        intersectsScreenRowRange: [startScreenRow, endScreenRow - 1]
      });
      const layerDecorations = this.layerDecorationsByMarkerLayer.get(
        markerLayer
      );
      const hasMarkerDecorations =
        this.markerDecorationCountsByLayer.get(markerLayer) > 0;

      for (let i = 0; i < markers.length; i++) {
        const marker = markers[i];
        if (!marker.isValid()) continue;

        let decorationPropertiesForMarker = decorationPropertiesByMarker.get(
          marker
        );
        if (decorationPropertiesForMarker == null) {
          decorationPropertiesForMarker = [];
          decorationPropertiesByMarker.set(
            marker,
            decorationPropertiesForMarker
          );
        }

        if (layerDecorations) {
          layerDecorations.forEach(layerDecoration => {
            const properties =
              layerDecoration.getPropertiesForMarker(marker) ||
              layerDecoration.getProperties();
            decorationPropertiesForMarker.push(properties);
          });
        }

        if (hasMarkerDecorations) {
          const decorationsForMarker = this.decorationsByMarker.get(marker);
          if (decorationsForMarker) {
            decorationsForMarker.forEach(decoration => {
              decorationPropertiesForMarker.push(decoration.getProperties());
            });
          }
        }
      }
    });

    return decorationPropertiesByMarker;
  }

  decorationsForScreenRowRange(startScreenRow, endScreenRow) {
    const decorationsByMarkerId = {};
    for (const layer of this.decorationCountsByLayer.keys()) {
      for (const marker of layer.findMarkers({
        intersectsScreenRowRange: [startScreenRow, endScreenRow]
      })) {
        const decorations = this.decorationsByMarker.get(marker);
        if (decorations) {
          decorationsByMarkerId[marker.id] = Array.from(decorations);
        }
      }
    }
    return decorationsByMarkerId;
  }

  decorationsStateForScreenRowRange(startScreenRow, endScreenRow) {
    const decorationsState = {};

    for (const layer of this.decorationCountsByLayer.keys()) {
      for (const marker of layer.findMarkers({
        intersectsScreenRowRange: [startScreenRow, endScreenRow]
      })) {
        if (marker.isValid()) {
          const screenRange = marker.getScreenRange();
          const bufferRange = marker.getBufferRange();
          const rangeIsReversed = marker.isReversed();

          const decorations = this.decorationsByMarker.get(marker);
          if (decorations) {
            decorations.forEach(decoration => {
              decorationsState[decoration.id] = {
                properties: decoration.properties,
                screenRange,
                bufferRange,
                rangeIsReversed
              };
            });
          }

          const layerDecorations = this.layerDecorationsByMarkerLayer.get(
            layer
          );
          if (layerDecorations) {
            layerDecorations.forEach(layerDecoration => {
              const properties =
                layerDecoration.getPropertiesForMarker(marker) ||
                layerDecoration.getProperties();
              decorationsState[`${layerDecoration.id}-${marker.id}`] = {
                properties,
                screenRange,
                bufferRange,
                rangeIsReversed
              };
            });
          }
        }
      }
    }

    return decorationsState;
  }

  decorateMarker(marker, decorationParams) {
    if (marker.isDestroyed()) {
      const error = new Error('Cannot decorate a destroyed marker');
      error.metadata = { markerLayerIsDestroyed: marker.layer.isDestroyed() };
      if (marker.destroyStackTrace != null) {
        error.metadata.destroyStackTrace = marker.destroyStackTrace;
      }
      if (
        marker.bufferMarker != null &&
        marker.bufferMarker.destroyStackTrace != null
      ) {
        error.metadata.destroyStackTrace =
          marker.bufferMarker.destroyStackTrace;
      }
      throw error;
    }
    marker = this.displayLayer
      .getMarkerLayer(marker.layer.id)
      .getMarker(marker.id);
    const decoration = new Decoration(marker, this, decorationParams);
    let decorationsForMarker = this.decorationsByMarker.get(marker);
    if (!decorationsForMarker) {
      decorationsForMarker = new Set();
      this.decorationsByMarker.set(marker, decorationsForMarker);
    }
    decorationsForMarker.add(decoration);
    if (decoration.isType('overlay')) this.overlayDecorations.add(decoration);
    this.observeDecoratedLayer(marker.layer, true);
    this.editor.didAddDecoration(decoration);
    this.emitDidUpdateDecorations();
    this.emitter.emit('did-add-decoration', decoration);
    return decoration;
  }

  decorateMarkerLayer(markerLayer, decorationParams) {
    if (markerLayer.isDestroyed()) {
      throw new Error('Cannot decorate a destroyed marker layer');
    }
    markerLayer = this.displayLayer.getMarkerLayer(markerLayer.id);
    const decoration = new LayerDecoration(markerLayer, this, decorationParams);
    let layerDecorations = this.layerDecorationsByMarkerLayer.get(markerLayer);
    if (layerDecorations == null) {
      layerDecorations = new Set();
      this.layerDecorationsByMarkerLayer.set(markerLayer, layerDecorations);
    }
    layerDecorations.add(decoration);
    this.observeDecoratedLayer(markerLayer, false);
    this.emitDidUpdateDecorations();
    return decoration;
  }

  emitDidUpdateDecorations() {
    this.editor.scheduleComponentUpdate();
    this.emitter.emit('did-update-decorations');
  }

  decorationDidChangeType(decoration) {
    if (decoration.isType('overlay')) {
      this.overlayDecorations.add(decoration);
    } else {
      this.overlayDecorations.delete(decoration);
    }
  }

  didDestroyMarkerDecoration(decoration) {
    const { marker } = decoration;
    const decorations = this.decorationsByMarker.get(marker);
    if (decorations && decorations.has(decoration)) {
      decorations.delete(decoration);
      if (decorations.size === 0) this.decorationsByMarker.delete(marker);
      this.overlayDecorations.delete(decoration);
      this.unobserveDecoratedLayer(marker.layer, true);
      this.emitter.emit('did-remove-decoration', decoration);
      this.emitDidUpdateDecorations();
    }
  }

  didDestroyLayerDecoration(decoration) {
    const { markerLayer } = decoration;
    const decorations = this.layerDecorationsByMarkerLayer.get(markerLayer);

    if (decorations && decorations.has(decoration)) {
      decorations.delete(decoration);
      if (decorations.size === 0) {
        this.layerDecorationsByMarkerLayer.delete(markerLayer);
      }
      this.unobserveDecoratedLayer(markerLayer, true);
      this.emitDidUpdateDecorations();
    }
  }

  observeDecoratedLayer(layer, isMarkerDecoration) {
    const newCount = (this.decorationCountsByLayer.get(layer) || 0) + 1;
    this.decorationCountsByLayer.set(layer, newCount);
    if (newCount === 1) {
      this.layerUpdateDisposablesByLayer.set(
        layer,
        layer.onDidUpdate(this.emitDidUpdateDecorations.bind(this))
      );
    }
    if (isMarkerDecoration) {
      this.markerDecorationCountsByLayer.set(
        layer,
        (this.markerDecorationCountsByLayer.get(layer) || 0) + 1
      );
    }
  }

  unobserveDecoratedLayer(layer, isMarkerDecoration) {
    const newCount = this.decorationCountsByLayer.get(layer) - 1;
    if (newCount === 0) {
      this.layerUpdateDisposablesByLayer.get(layer).dispose();
      this.decorationCountsByLayer.delete(layer);
    } else {
      this.decorationCountsByLayer.set(layer, newCount);
    }
    if (isMarkerDecoration) {
      this.markerDecorationCountsByLayer.set(
        this.markerDecorationCountsByLayer.get(layer) - 1
      );
    }
  }
};
