local lgi = require 'lgi'
local gtk = lgi.Gtk
local gdk = lgi.Gdk
local editor = lgi.GtkSource

local utils = require 'utils'
local class = utils.class

local Editor = class()
function Editor:init()
  self.buffer = self.buffer -- editor.Buffer,
  self.view = self.view -- editor.View
end

function Editor:prev_page()
end
function Editor:next_page()
end
function Editor:prev_item()
end
function Editor:next_item()
end

-- gspell

return Editor
