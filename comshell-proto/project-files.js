// https://github.com/matthew-matvei/freeman
// node-watch
// https://github.com/ikorchenov/atom-open-folder

/*
https://github.com/ajaxorg/ace
https://ace.c9.io/#nav=api
https://ace.c9.io/build/kitchen-sink.html
https://stackoverflow.com/questions/24653020/does-ace-editor-have-an-api-similar-to-codemirrors-addlinewidget
  https://github.com/ajaxorg/ace/blob/master/lib/ace/line_widgets.js

https://codemirror.net/doc/manual.html
https://github.com/sagemathinc/cocalc/issues/2796
https://github.com/assisrafael/codemirror-addon-indent-guide
https://github.com/jupyterlab/jupyterlab/tree/master/packages/codemirror
  https://github.com/jupyterlab/jupyterlab/tree/master/packages/codeeditor
  https://github.com/jupyterlab/jupyterlab/tree/master/packages/fileeditor

https://microsoft.github.io/monaco-editor/api/index.html
contentWidget
https://microsoft.github.io/monaco-editor/playground.html#interacting-with-the-editor-listening-to-mouse-events

https://github.com/arnog/mathlive
https://github.com/mathquill/mathquill
*/

// https://github.com/sachinchoolur/lightgallery.js

const path = require('path');
const fs = require('fs');
const projectViews = require('./project-views');

module.exports =
class ProjectFiles extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({mode: 'open'});

    const wrapper = document.createElement('div');
    wrapper.setAttribute('class','wrapper');

    const style = document.createElement('style');
    style.textContent = `.wrapper {
      flex: 1 1 0;
    }`

    this.shadowRoot.append(style, wrapper);
  }

  // indicate open files;
  // move between open files;
}

window.customElements.define('project-files', ProjectFiles);
