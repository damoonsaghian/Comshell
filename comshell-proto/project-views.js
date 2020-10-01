const projectViews = document.createElement('div');
module.exports = projectViews;

projectViews.setAttribute('class', 'project-views');

projectViews.views = {};

projectViews.initialize = (userDataDir) => {
  loadSession(userDataDir);

  // set a timer to save session

  nw.Window.get().on('close', async () => {
    await saveSession(userDataDir);
    win.close(true);
  });
}

projectViews.addView = (view) => {}

function loadSession (userDataDir) {
  // if there is a saved session file for the project
}

async function saveSession(userDataDir) => {}

/*
https://github.com/ajaxorg/ace
https://ace.c9.io/#nav=api
https://ace.c9.io/build/kitchen-sink.html
https://stackoverflow.com/questions/24653020/does-ace-editor-have-an-api-similar-to-codemirrors-addlinewidget
  https://github.com/ajaxorg/ace/blob/master/lib/ace/line_widgets.js

https://codemirror.net/doc/manual.html
https://github.com/sagemathinc/cocalc/issues/2796
https://github.com/assisrafael/codemirror-addon-indent-guide

https://microsoft.github.io/monaco-editor/api/index.html
contentWidget
https://microsoft.github.io/monaco-editor/playground.html#interacting-with-the-editor-listening-to-mouse-events

https://github.com/arnog/mathlive
https://github.com/mathquill/mathquill
*/
