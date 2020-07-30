let nextInstanceId = 1;

module.exports = class Model {
  static resetNextInstanceId() {
    return nextInstanceId = 1;
  }

  static alive = true;

  constructor(params) {
    this.assignId(params?.id);
  }

  assignId(id) {
    if (this.id == null) {
      this.id = id != null ? id : nextInstanceId++;
    }
    if (id >= nextInstanceId) {
      nextInstanceId = id + 1;
    }
  }

  destroy() {
    if (!this.isAlive()) { return; }
    this.alive = false;
    return typeof this.destroyed === "function" ? this.destroyed() : undefined;
  }

  isAlive() { return this.alive; }

  isDestroyed() { return !this.isAlive(); }
}
