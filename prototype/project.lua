require 'gallery'
require 'editor'
require 'files_tree'
require 'web_browser'

local View = class {
  item = nil, -- Gallery or Editor
  files_tree = nil, -- FilesTree
  floating_layer = nil, -- gtk.Container
}

Project = class {
  self.views = nil, -- {View}
  self.notebook = nil, -- gtk.NoteBook
  self.web_browsers = nil, -- { "url" = WebBrowser }
  self.external_projects = nil -- { "url" = Project }

go_to_chapter = function(chapter_path)
end

prev_chapter = function()
end

next_chapter = function()
end
}
