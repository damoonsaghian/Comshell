const { CompositeDisposable } = require('event-kit');

function iconClassForPath(filePath) {
  const extension = path.extname(filePath);

  if (fs.isSymbolicLinkSync(filePath)) {
    return 'icon-file-symlink-file';
  } else if (fs.isReadmePath(filePath)) {
    return 'icon-book';
  } else if (fs.isCompressedExtension(extension)) {
    return 'icon-file-zip';
  } else if (fs.isImageExtension(extension)) {
    return 'icon-file-media';
  } else if (fs.isPdfExtension(extension)) {
    return 'icon-file-pdf';
  } else if (fs.isBinaryExtension(extension)) {
    return 'icon-file-binary';
  } else {
    return 'icon-file-text';
  }
}

module.exports =
class FileView {
  constructor (file) {
    this.file = file;
    this.subscriptions = new CompositeDisposable();
    this.subscriptions.add(this.file.onDidDestroy(
      () => this.subscriptions.dispose()
    ));

    this.element = document.createElement('li');
    this.element.setAttribute('is', 'tree-view-file');
    this.element.draggable = true;
    this.element.classList.add('file', 'entry', 'list-item');

    this.fileName = document.createElement('span');
    this.fileName.classList.add('name', 'icon');
    this.element.appendChild(this.fileName);
    this.fileName.textContent = this.file.name;
    this.fileName.title = this.file.name;
    this.fileName.dataset.name = this.file.name;
    this.fileName.dataset.path = this.file.path;

    this.updateIcon();
    this.subscriptions.add(this.file.onDidStatusChange(
      () => this.updateStatus()
    ));
    this.updateStatus();
  }

  updateIcon () {
    this.fileName.className = '';

    const classes = ['name', 'icon'];
    let iconClass = iconClassForPath(this.file.path, 'tree-view');

    if (iconClass) {
      if (!Array.isArray(iconClass)) {
        iconClass = iconClass.toString().split(/\s+/g);
      }
      classes.push(...iconClass);
    }
    this.fileName.classList.add(...classes);

    this.element.getPath = this.getPath.bind(this)
    this.element.isPathEqual = this.isPathEqual.bind(this)
    this.element.file = this.file
    this.element.fileName = this.fileName
    this.element.updateStatus = this.updateStatus.bind(this)
  }

  updateStatus () {
    this.element.classList.remove(
      'status-ignored', 'status-ignored-name', 'status-modified', 'status-added'
    );
    if (this.file.status != null)
      this.element.classList.add(`status-${this.file.status}`);
  }

  getPath () {
    return this.fileName.dataset.path;
  }

  isPathEqual (pathToCompare) {
    return this.file.isPathEqual(pathToCompare);
  }
}
