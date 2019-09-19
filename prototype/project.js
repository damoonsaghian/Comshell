const { Gio, Gdk, Gtk } = imports.gi;
const { Gallery } = imports.gallery;
const { Editor } = imports.editor;
const { FilesTree } = imports.files_tree;
const { WebBrowser } = imports.web_browser;

class View {
  // Gallery or Editor, FilesTree, Gtk.Container
  constructor(item, files_tree, floating_layer) {}
}

class Project {
  // [View], Gtk.NoteBook, { "url": WebBrowser }, { "url": Project }
  constructor(views, notebook, web_browsers, external_projects) {}

  go_to_chapter(chapter_path) {}

  prev_chapter() {}

  next_chapter() {}
}
