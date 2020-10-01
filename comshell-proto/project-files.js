// https://github.com/maxogden/tree-view
// https://github.com/zcbenz/nw-sample-apps/tree/master/file-explorer
// https://github.com/matthew-matvei/freeman
// node-watch

// https://github.com/ikorchenov/atom-open-folder
// https://github.com/hswolff/tree-view-extended

// monaco-editor
// Ace editor
// Quill
// https://github.com/sachinchoolur/lightgallery.js

const path = require('path');
const fs = require('fs');
const projectViews = require('./project-views');

const projectFiles = document.createElement('div');
module.exports = projectFiles;

projectFiles.initialize = (userDataDir) => {
  const projectDir = userDataDir ? path.join(userDataDir, '../..') : null;

  projectViews.initialize(userDataDir);

  document.appendChild(projectFiles);
  document.appendChild(projectViews);
}

// indicate open files by an underline;
// move between open files;
