local lgi = require 'lgi'
local gtk = lgi.Gtk
local gdk = lgi.Gdk
local editor = lgi.GtkSource

local utils = require 'utils'
local class = utils.class

local Editor = class {
  buffer = nil, -- editor.Buffer,
  view = nil, -- editor.View

  prev_page = function(self)
  end,

  next_page = function(self)
  end,

  prev_item = function(self)
  end,

  next_item = function(self)
  end
}
-- gspell

return Editor
