require "gallery"
require "editor"
require "files_tree"
require "web_browser"

local View = class()
function View:init(defaults)
  self.item = defaults.item -- Gallery or Editor
  self.files_tree = defaults.files_tree -- FilesTree
  self.floating_layer = gtk.Container()
end

Project = class()
function Project:init(defaults)
  self.views = {} -- {View}
  self.notebook = gtk.NoteBook()
  self.web_browsers = {} -- { "url" = WebBrowser }
  self.external_projects = {} -- { "url" = Project }
end

function Projecy:go_to_chapter(chapter_path)
end

function Projecy:prev_chapter()
end

function Projecy:next_chapter()
end
