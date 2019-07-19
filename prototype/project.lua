local lgi = require 'lgi'
local gtk = lgi.Gtk
local gdk = lgi.Gdk

local utils = require 'utils'
local class = utils.class

local Gallery = require 'gallery'
local Editor = require 'editor'
local FilesTree = require 'files_tree'
local WebBrowser = require 'web_browser'

local View = class()
function View:init()
  self.item = self.item -- Gallery or Editor
  self.files_tree = self.files_tree -- FilesTree
  self.floating_layer = self.floating_layer -- gtk.Container
end

local Project = class()
function Project:init()
  self.views = self.views -- {View}
  self.notebook = self.notebook -- gtk.NoteBook
  self.web_browsers = self.web_browsers -- { "url" = WebBrowser }
  self.external_projects = self.external_projects -- { "url" = Project }
end

function Project:go_to_chapter(chapter_path)
end

function Project:prev_chapter()
end

function Project:next_chapter()
end

return Project
