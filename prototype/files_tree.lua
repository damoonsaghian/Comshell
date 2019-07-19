local lgi = require 'lgi'
local gtk = lgi.Gtk
local gdk = lgi.Gdk

local utils = require 'utils'
local class = utils.class

-- https://github.com/jonathanBieler/GtkIDE.jl/blob/master/src/sidepanels/FilesPanel.jl
-- https://gitlab.gnome.org/GNOME/shotwell/blob/master/src/sidebar/Tree.vala
-- https://github.com/teejee2008/polo/blob/master/src/Gtk/FileViewList.vala

local FilesTree = class()

return FilesTree
