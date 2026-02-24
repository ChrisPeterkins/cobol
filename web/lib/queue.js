const MAX_QUEUE_SIZE = 10;

class SerialQueue {
  constructor() {
    this._chain = Promise.resolve();
    this._pending = 0;
  }

  get pending() {
    return this._pending;
  }

  enqueue(fn) {
    if (this._pending >= MAX_QUEUE_SIZE) {
      return Promise.reject(new Error('Queue full — server busy'));
    }
    this._pending++;
    const task = this._chain
      .then(() => fn())
      .finally(() => { this._pending--; });
    // Keep the chain going even if the task rejects
    this._chain = task.catch(() => {});
    return task;
  }
}

module.exports = new SerialQueue();
