local lgi = require 'lgi'
local gtk = lgi.Gtk
local gdk = lgi.Gdk

local utils = require 'utils'
local class = utils.class

local Gallery = require 'gallery'
local Editor = require 'editor'
local FilesTree = require 'files_tree'
local WebBrowser = require 'web_browser'

local View = class {
  item = nil, -- Gallery or Editor
  files_tree = nil, -- FilesTree
  floating_layer = nil, -- gtk.Container
}

local Project = class {
  self.views = nil, -- {View}
  self.notebook = nil, -- gtk.NoteBook
  self.web_browsers = nil, -- { "url" = WebBrowser }
  self.external_projects = self.external_projects -- { "url" = Project }

go_to_chapter = function(chapter_path)
end

prev_chapter = function()
end

next_chapter = function()
end
}
return Project
