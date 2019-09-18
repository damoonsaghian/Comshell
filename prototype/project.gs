let { GLib, Gio, Gdk, Gtk } = imports.gi
let { Gallery } = imports.gallery
let { Editor } = imports.editor
let { FilesTree } = imports.files_tree
let { WebBrowser } = imports.web_browser

class View
  // Gallery or Editor, FilesTree, Gtk.Container
  def constructor(@item, @files_tree, @floating_layer)

class Project
  // [View], Gtk.NoteBook, { "url": WebBrowser }, { "url": Project }
  def constructor(@views, @notebook, @web_browsers, @external_projects)

  def go_to_chapter(@chapter_path)

  def prev_chapter()

  def next_chapter()
