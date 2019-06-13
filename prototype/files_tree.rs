use std::collections::HashMap;
use may_actor::Actor;

use std::{rc::Rc, cell::RefCell};
use main::do_in_main;
use gtk::{self, prelude::*};
use gdk::enums::key;

// https://github.com/daa84/neovim-gtk/blob/master/src/file_browser.rs
// https://github.com/jonathanBieler/GtkIDE.jl/blob/master/src/sidepanels/FilesPanel.jl
// https://gitlab.gnome.org/GNOME/shotwell/blob/master/src/sidebar/Tree.vala
// https://github.com/teejee2008/polo/blob/master/src/Gtk/FileViewList.vala
// https://gitlab.gnome.org/GNOME/gnome-builder/tree/master/src/libide/tree

pub struct FilesTree {}
