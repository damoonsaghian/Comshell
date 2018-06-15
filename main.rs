extern crate futures;
extern crate futures_cpupool;
extern crate gtk;

use futures::Future;
use futures::stream::Stream;
use futures_cpupool::CpuPool;

struct OpenedProject {
    name: &str,
    index: i8, // index in GTK stack widget
    notebook: gtk::Notebook,
  
}

struct UIState {
    stack: gtk::Stack,
    opened_projects: Hash<&str, OpenedProject>,
    current_project: ,
    current_tab: ,
  
  
}

impl UIState {
    fn show_projects_list() {
    }
    fn go_to_project() {
    }
    fn open_tab() {
    }
    fn prev_tab() {
    }
    fn next_tab() {
    }
    fn go_to_chapter() {
    }
    fn prev_chapter() {
    }
    fn next_chapter() {
    }
    fn go_to_page() {
    }
    fn prev_page() {
    }
    fn next_page() {
    }
    fn prev_item() {
    }
    fn next_item() {
    }
}

fn main() {
    // set up a thread pool
    let pool = CpuPool::new_num_cpus();
    
}

/*
gi.repository
Gio, Gdk, Gtk, GtkSource, Gst, GstVideo

struct EditorView
    editor::GtkSource.View
end

struct GalleryView
    scrolled_window::Gtk.ScrolledWindow
    galley::Gtk.FlowBox
end

struct ProjectView
    # tabs which contain an EditorView or a GalleryView or a WebView
    tabs::
    open_tab::Int
end

struct ComshellWindow
    window::Gtk.Window
    # the main workspace area (stack of opened projects)
    workspace = Gtk.Stack()
    open_project::Int
end

display = Gdk.Display.get_default()
  
function new_doc(project::ProjectView)
    doc = Gtk.TextEditor(expand: True)
    doc.set_expand(true)
    tab = Gtk.Box()
    tab.set_orientation("VERTICAL")
    tab.add(doc)
end



# a view showing the list of projects
projects_list = GtkTreeView()
    


# todo: multi screen support
# inside each screen create a Comshell window.
# move between them with Gtk.Window.present().
# opened tabs, text buffers (and webview buffer?), and watched directories are shared between windows.
window = Gtk.Window()
window.connect("delete-event", Gtk.main_quit)
window.add(workspace)
# set windows size to fill the screen
rectangle = display.get_monitor(monitor_n).get_geometry()
window.show_all()
window.move(rectangle.x, rectangle.y)
window.resize(rectangle.width, rectangle.height)
window.maximize() # to have a proper window, if there is a window manager (with panels ...).




# the stack widget whose children are the open projects, as well as the project list itself (which is the first child)
projects_stack = Gtk.Stack()
projects_stack.add_named(projects_list, "projects")

Gtk.main()


# https://wiki.archlinux.org/index.php/Udisks#udevadm_monitor

# GIO: files, drives, volumes, streamIO, network
# evince poopler, telepathy farstream, goocanvas
*/

/*
winit
evdev (for input) and drm (to access the framebuffer)

text editing:
"https://github.com/jmacdonald/scribe"
"https://github.com/mathall/rim"
"https://github.com/gchp/iota"

web:
"https://github.com/servo/rust-url/"
"https://crates.io/crates/hyper"
    "https://crates.io/crates/hyper-rustls"
    "https://crates.io/crates/reqwest"
    "https://crates.io/crates/simplist"
"https://crates.io/crates/httpbin"
"https://crates.io/crates/httpbis"
"https://crates.io/crates/xmpp-proto"
"https://crates.io/crates/pijul" (version control system)

media:
"https://github.com/pcwalton/rust-media"
"https://github.com/meh/rust-ffmpeg"
"https://crates.io/crates/mpv"
"https://github.com/tomaka/rodio"
"https://github.com/tomaka/cpal"
"https://crates.io/crates/lewton"
"https://crates.io/crates/camera_capture"
*/
