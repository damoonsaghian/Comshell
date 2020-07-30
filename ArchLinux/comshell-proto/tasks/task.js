const _ = require('underscore-plus');
const ChildProcess = require('child_process');
const { Emitter } = require('event-kit');

// Extended: Run a node script in a separate process.
//
// Used by the fuzzy-finder and [find in project](https://github.com/atom/atom/blob/master/src/scan-handler.coffee).
//
// For a real-world example, see the [scan-handler](https://github.com/atom/atom/blob/master/src/scan-handler.coffee)
// and the [instantiation of the task](https://github.com/atom/atom/blob/4a20f13162f65afc816b512ad7201e528c3443d7/src/project.coffee#L245).
//
// ## Examples
//
// In your package code:
//
// ```coffee
// {Task} = require 'atom'
//
// task = Task.once '/path/to/task-file.coffee', parameter1, parameter2, ->
//   console.log 'task has finished'
//
// task.on 'some-event-from-the-task', (data) =>
//   console.log data.someString # prints 'yep this is it'
// ```
//
// In `'/path/to/task-file.coffee'`:
//
// ```coffee
// module.exports = (parameter1, parameter2) ->
//   # Indicates that this task will be async.
//   # Call the `callback` to finish the task
//   callback = @async()
//
//   emit('some-event-from-the-task', {someString: 'yep this is it'})
//
//   callback()
// ```
module.exports = class Task {
  // Public: A helper method to easily launch and run a task once.
  //
  // * `taskPath` The {String} path to the CoffeeScript/JavaScript file which
  //   exports a single {Function} to execute.
  // * `args` The arguments to pass to the exported function.
  //
  // Returns the created {Task}.
  static once(taskPath, ...args) {
    const task = new Task(taskPath);
    task.once('task:completed', () => task.terminate());
    task.start(...args);
    return task;
  }

  // Called upon task completion.
  //
  // It receives the same arguments that were passed to the task.
  //
  // If subclassed, this is intended to be overridden. However if {::start}
  // receives a completion callback, this is overridden.
  static callback = null;

  // Public: Creates a task. You should probably use {.once}
  //
  // * `taskPath` The {String} path to the CoffeeScript/JavaScript file that
  //   exports a single {Function} to execute.
  constructor(taskPath) {
    this.emitter = new Emitter;

    taskPath = require.resolve(taskPath);

    const env = Object.assign({}, process.env, { userAgent: navigator.userAgent });
    this.childProcess = ChildProcess.fork(
      require.resolve('./task-bootstrap'),
      [taskPath], { env, silent: true }
    );

    this.on("task:log", () => console.log(...arguments));
    this.on("task:warn", () => console.warn(...arguments));
    this.on("task:error", () => console.error(...arguments));
    this.on(
      "task:completed",
      (...args) => if (typeof this.callback === "function") {this.callback(...args)}
    );

    this.handleEvents();
  }

  // Routes messages from the child to the appropriate event.
  handleEvents() {
    // TodoElectronIssue: removeAllListeners() without arguments does not work on electron v3.
    // Remove the argument when migrating to electron v4.
    this.childProcess.removeAllListeners('message');
    this.childProcess.on('message', ({ event, args }) => {
      if (this.childProcess != null)
        this.emitter.emit(event, args);
    });

    // Catch the errors that happened before task-bootstrap.
    if (this.childProcess.stdout != null) {
      // TodoElectronIssue: removeAllListeners() without arguments does not work on electron v3.
      // Remove the argument when migrating to electron v4.
      this.childProcess.stdout.removeAllListeners('data');
      this.childProcess.stdout.on('data', (data) => console.log(data.toString()));
    }

    if (this.childProcess.stderr != null) {
      // TodoElectronIssue: removeAllListeners() without arguments does not work on electron v3.
      // Remove the argument when migrating to electron v4.
      this.childProcess.stderr.removeAllListeners('data');
      this.childProcess.stderr.on('data', (data) => console.error(data.toString()));
    }
  }

  // Public: Starts the task.
  //
  // Throws an error if this task has already been terminated or if sending a
  // message to the child process fails.
  //
  // * `args` The arguments to pass to the function exported by this task's script.
  // * `callback` (optional) A {Function} to call when the task completes.
  start(...args, callback) {
    if (this.childProcess == null)
      throw new Error('Cannot start terminated process');

    this.handleEvents();
    if (_.isFunction(callback)) {
      this.callback = callback;
    } else {
      args.push(callback);
    }
    this.send({ event: 'start', args });
  }

  // Public: Send message to the task.
  //
  // Throws an error if this task has already been terminated or if sending a
  // message to the child process fails.
  //
  // * `message` The message to send to the task.
  send(message) {
    if (this.childProcess != null) {
      this.childProcess.send(message);
    } else {
      throw new Error('Cannot send message to terminated process');
    }
  }

  // Public: Call a function when an event is emitted by the child process
  //
  // * `eventName` The {String} name of the event to handle.
  // * `callback` The {Function} to call when the event is emitted.
  //
  // Returns a {Disposable} that can be used to stop listening for the event.
  on(eventName, callback) {
    this.emitter.on(eventName, (args) => callback(...args));
  }

  once(eventName, callback) {
    const disposable = this.on(eventName, (...args) => {
      disposable.dispose();
      callback(...args);
    });
  }

  // Public: Forcefully stop the running task.
  //
  // No more events are emitted once this method is called.
  terminate() {
    if (this.childProcess == null) { return false; }

    // TodoElectronIssue: removeAllListeners() without arguments does not work on electron v3.
    // Remove the argument when migrating to electron v4.
    this.childProcess.removeAllListeners('message');
    this.childProcess.stdout?.removeAllListeners('data');
    this.childProcess.stderr?.removeAllListeners('data');
    this.childProcess.kill();
    this.childProcess = null;

    return true;
  }

  // Public: Cancel the running task and emit an event if it was canceled.
  //
  // Returns a {Boolean} indicating whether the task was terminated.
  cancel() {
    const didForcefullyTerminate = this.terminate();
    if (didForcefullyTerminate)
      this.emitter.emit('task:cancelled');
    return didForcefullyTerminate;
  }
}
