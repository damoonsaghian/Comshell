const Token = require('./token');
const CommentScopeRegex = /(\b|\.)comment/;

const idCounter = 1;

module.exports = class TokenizedLine {
  constructor(properties) {
    this.id = idCounter++;

    if (properties === null) { return; }
    this.openScopes = properties.openScopes;
    this.text = properties.text;
    this.tags = properties.tags;
    this.ruleStack = properties.ruleStack;
    this.tokenIterator = properties.tokenIterator;
    this.grammar = properties.grammer;
    this.cachedTokens = properties.tokens;
  }

  getTokenIterator() {
    return this.tokenIterator.reset(this);
  }

  tokenAtBufferColumn(bufferColumn) {
    return this.tokens[this.tokenIndexAtBufferColumn(bufferColumn)];
  }

  tokenIndexAtBufferColumn(bufferColumn) {
    let column = 0;
    this.tokens.forEach((token, index) => {
      column += token.value.length;
      if (column > bufferColumn) { return index; }
    });
    return index - 1;
  }

  tokenStartColumnForBufferColumn(bufferColumn) {
    let delta = 0;
    let nextDelta;
    for (const token of this.tokens) {
      nextDelta = delta + token.bufferDelta;
      if (nextDelta > bufferColumn) { break; }
      delta = nextDelta;
    }
    return delta;
  }

  isComment() {
    if (this?.isCommentLine !== undefined) { return this.isCommentLine }

    this.isCommentLine = false;

    for (const tag of this.openScopes) {
      if (this.isCommentOpenTag(tag)) {
        this.isCommentLine = true;
        return this.isCommentLine;
      }
    }

    let startIndex = 0;
    for (const tag of this.tags) {
      // if we haven't encountered any comment scope when reading the first
      // non-whitespace chunk of text, then we consider this as not being a
      // comment line;
      if (tag > 0) {
        if (!isWhitespaceOnly(this.text.substr(startIndex, tag))) { break; }
        startIndex += tag;
      }

      if (this.isCommentOpenTag(tag)) {
        this.isCommentLine = true;
        return this.isCommentLine;
      }
    }

    return this.isCommentLine;
  }

  isCommentOpenTag(tag) {
    if (tag < 0 && (tag & 1) === 1) {
      const scope = this.grammar.scopeForId(tag);
      if (CommentScopeRegex.test(scope)) { return true }
    }
    return false;
  }

  tokenAtIndex(index) {
    return this.tokens[index];
  }

  getTokenCount() {
    let count = 0;
    for (const tag of this.tags) {
      if (tag >= 0) count++;
    }
    return count;
  }
}

Object.defineProperty(TokenizedLine.prototype, 'tokens', {
  get: function() {
    if (this.cachedTokens) {
      return this.cachedTokens;
    } else {
      const iterator = this.getTokenIterator();
      const tokens = [];
      while (iterator.next()) {
        tokens.push(new Token({
          value: iterator.getText(),
          scopes: iterator.getScopes().slice()
        }));
      }
      return tokens;
    }
  }
});

function isWhitespaceOnly(text) {
  for (const char of text) {
    if (char !== '\t' && char !== ' ') {
      return false;
    }
  }
  return true;
}
