pub fn do_in_main_thread<F>(func: F)
  where F: FnOnce()
{
  use glib::source;
  source::idle_add(|| {
    func();
    source::Continue(false)
  });
}
