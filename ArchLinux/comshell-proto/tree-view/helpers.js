const path = require('path');

module.exports = {
  repoForPath: (goalPath) => {
    atom.project.getPaths().forEach((projectPath, i) => {
      if (goalPath === projectPath || goalPath.indexOf(projectPath + path.sep) === 0)
        return atom.project.getRepositories()[i];
    });
    return null;
  },

  getStyleObject: (el) => {
    const styleProperties = window.getComputedStyle(el);
    const styleObject = {};
    for (const property in styleProperties) {
      const value = styleProperties.getPropertyValue(property);
      const camelizedAttr = property.replace(/\-([a-z])/g, (a, b) => b.toUpperCase());
      styleObject[camelizedAttr] = value;
    }
    return styleObject;
  },

  getFullExtension: (filePath) => {
    const basename = path.basename(filePath);
    const position = basename.indexOf('.');
    if (position > 0) {
      return basename.slice(position);
    } else {
      return '';
    }
  }
}
