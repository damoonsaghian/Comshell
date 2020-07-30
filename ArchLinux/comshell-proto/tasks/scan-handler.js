const path = require("path");
const async = require("async");
const { PathSearcher, PathScanner, search } = require('scandal');

const processPaths = (rootPath, paths) => {
  if (!(paths?.length > 0)) { return paths; }

  const rootPathBase = path.basename(rootPath);
  const results = [];
  for (const givenPath of paths) {
    const segments = givenPath.split(path.sep);
    const firstSegment = segments.shift();
    results.push(givenPath);
    if (firstSegment === rootPathBase) {
      if (segments.length === 0) {
        results.push(path.join("**", "*"));
      } else {
        results.push(path.join(...segments));
      }
    }
  }
  return results;
};

module.exports = function(rootPaths, regexSource, options, searchOptions={}) {
  const callback = this.async();

  const PATHS_COUNTER_SEARCHED_CHUNK = 50;
  let pathsSearched = 0;

  const searcher = new PathSearcher(searchOptions);

  searcher.on('file-error', ({code, path, message}) =>
    emit('scan:file-error', {code, path, message})
  );

  searcher.on('results-found', (result) =>
    emit('scan:result-found', result)
  );

  let flags = "g";
  if (options.ignoreCase) flags += "i";
  const regex = new RegExp(regexSource, flags);

  async.each(rootPaths, function(rootPath, next) {
    const options2 = Object.assign({}, options, {
      inclusions: processPaths(rootPath, options.inclusions),
      globalExclusions: processPaths(rootPath, options.globalExclusions)
    });

    const scanner = new PathScanner(rootPath, options2);
    scanner.on('path-found', () => {
      pathsSearched++;
      if (pathsSearched % PATHS_COUNTER_SEARCHED_CHUNK === 0)
        emit('scan:paths-searched', pathsSearched);
    });

    return search(regex, scanner, searcher, () => {
      emit('scan:paths-searched', pathsSearched);
      return next();
    });
  }, callback);
}
