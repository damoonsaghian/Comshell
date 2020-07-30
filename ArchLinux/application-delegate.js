const { Emitter, Disposable } = require('event-kit');
const nw = require('nw');

const getWindowLoadSettings = () => ({
  appName: 'Comshe',
  resourcePath: __dirname,
  atomHome: process.env.ATOM_HOME,
});
