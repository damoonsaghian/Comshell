// code extracted from:
// https://github.com/paradoxxxzero/gnome-shell-system-monitor-applet

const Clutter = imports.gi.Clutter;
const GLib = imports.gi.GLib;
const Gio = imports.gi.Gio;
const Shell = imports.gi.Shell;
const St = imports.gi.St;
var GTop, NM;

(() => {

try {
  GTop = imports.gi.GTop;
  NM = imports.gi.NM;
} catch (e) {
  log('[system monitor] catched error: ' + e);
  return;
}

const main = imports.ui.main;
const mainloop = imports.mainloop;

function parse_bytearray(byteArray) {
  const string = imports.byteArray.toString(byteArray);
  if (!string.match(/GjsModule byteArray/))
    return string;
  return byteArray;
}

function color_from_string(color) {
  let clutterColor, res;
  if (!Clutter.Color.from_string) {
    clutterColor = new Clutter.Color();
    clutterColor.from_string(color);
  } else {
    [res, clutterColor] = Clutter.Color.from_string(color);
  }
  return clutterColor;
}

class Chart {
  constructor(width, height, parent) {
    this.actor = new St.DrawingArea({ style: 'padding: 0 2px; margin: 3px 0;', reactive: false });
    this.parentC = parent;
    this.actor.set_width(this.width = width);
    this.actor.set_height(this.height = height);
    this.actor.connect('repaint', this._draw.bind(this));
    this.data = [];
    for (let i = 0; i < this.parentC.colors.length; i++)
      this.data[i] = [];
  }
  update() {
    let data_a = this.parentC.vals;
    if (data_a.length !== this.parentC.colors.length) return;
    let accdata = [];
    for (let l = 0; l < data_a.length; l++) {
      accdata[l] = (l === 0) ? data_a[0] : accdata[l - 1] + ((data_a[l] > 0) ? data_a[l] : 0);
      this.data[l].push(accdata[l]);
      if (this.data[l].length > this.width) {
        this.data[l].shift();
      }
    }
    if (!this.actor.visible) return;
    this.actor.queue_repaint();
  }
  _draw() {
    if (!this.actor.visible) return;
    let [width, height] = this.actor.get_surface_size();
    let cr = this.actor.get_context();
    let max;
    if (this.parentC.max) {
      max = this.parentC.max;
    } else {
      max = Math.max.apply(this, this.data[this.data.length - 1]);
      max = Math.max(1, Math.pow(2, Math.ceil(Math.log(max) / Math.log(2))));
    }
    Clutter.cairo_set_source_color(cr, color_from_string('#ffffff16'));
    cr.rectangle(0, 0, width, height);
    cr.fill();
    for (let i = this.parentC.colors.length - 1; i >= 0; i--) {
      cr.moveTo(width, height);
      for (let j = this.data[i].length - 1; j >= 0; j--) {
        cr.lineTo(width - (this.data[i].length - 1 - j), (1 - this.data[i][j] / max) * height);
      }
      cr.lineTo(width - (this.data[i].length - 1), height);
      cr.closePath();
      Clutter.cairo_set_source_color(cr, this.parentC.colors[i]);
      cr.fill();
    }
    cr.$dispose();
  }
  resize(schema, key) {
    let old_width = this.width;
    this.width = 40;
    if (old_width === this.width) return;
    this.actor.set_width(this.width);
    if (this.width < this.data[0].length) {
      for (let i = 0; i < this.parentC.colors.length; i++) {
        this.data[i] = this.data[i].slice(-this.width);
      }
    }
  }
}

// class to deal with volumes insertion / ejection
class MountsMonitor {
  constructor() {
    this.files = [];
    this.num_mounts = -1;
    this.listeners = [];
    this.connected = false;
    this._volumeMonitor = Gio.VolumeMonitor.get();
    const sys_mounts = ['/home', '/tmp', '/boot', '/usr', '/usr/local'];
    this.base_mounts = ['/'];
    sys_mounts.forEach((sMount) => {
      if (this.is_sys_mount(sMount + '/'))
        this.base_mounts.push(sMount);
    });
    this.connect();
  }
  refresh() {
    this.mounts = [];
    for (let base in this.base_mounts)
      this.mounts.push(this.base_mounts[base]);
    const mount_lines = this._volumeMonitor.get_mounts();
    mount_lines.forEach((mount) => {
      if (!this.is_net_mount(mount) && !this.is_ro_mount(mount)) {
        const mpath = mount.get_root().get_path() || mount.get_default_location().get_path();
        if (mpath) this.mounts.push(mpath);
      }
    });

    for (let i in this.listeners)
      this.listeners[i](this.mounts);
  }
  add_listener(cb) {
    this.listeners.push(cb);
  }
  remove_listener(cb) {
    this.listeners.pop(cb);
  }
  get_mounts() {
    return this.mounts;
  }
  is_sys_mount(mpath) {
    let file = Gio.file_new_for_path(mpath);
    let info = file.query_info(
      Gio.FILE_ATTRIBUTE_UNIX_IS_MOUNTPOINT,
      Gio.FileQueryInfoFlags.NONE,
      null
    );
    return info.get_attribute_boolean(Gio.FILE_ATTRIBUTE_UNIX_IS_MOUNTPOINT);
  }
  is_ro_mount(mount) {
    // FIXME: running this function after "login after waking from suspend"
    // can make login hang. Actual issue seems to occur when a former net
    // mount got broken (e.g. due to a VPN connection terminated or
    // otherwise broken connection)
    try {
      let file = mount.get_default_location();
      let info = file.query_filesystem_info(Gio.FILE_ATTRIBUTE_FILESYSTEM_READONLY, null);
      return info.get_attribute_boolean(Gio.FILE_ATTRIBUTE_FILESYSTEM_READONLY);
    } catch (e) {
      return false;
    }
  }
  is_net_mount(mount) {
    try {
      let file = mount.get_default_location();
      let info = file.query_filesystem_info(Gio.FILE_ATTRIBUTE_FILESYSTEM_TYPE, null);
      let result = info.get_attribute_string(Gio.FILE_ATTRIBUTE_FILESYSTEM_TYPE);
      let net_fs = ['nfs', 'smbfs', 'cifs', 'ftp', 'sshfs', 'sftp', 'mtp', 'mtpfs'];
      return !file.is_native() || net_fs.indexOf(result) > -1;
    } catch (e) {
      return false;
    }
  }
  connect() {
    if (this.connected) return;
    try {
      this.manager = this._volumeMonitor;
      this.mount_added_id = this.manager.connect('mount-added', this.refresh.bind(this));
      this.mount_removed_id = this.manager.connect('mount-removed', this.refresh.bind(this));
      // need to add the other signals here
      this.connected = true;
    } catch (e) {
      log('[System monitor] failed to register on placesManager notifications');
      log('[System monitor] got exception : ' + e);
    }
    this.refresh();
  }
  disconnect() {
    if (!this.connected) return;
    this.manager.disconnect(this.mount_added_id);
    this.manager.disconnect(this.mount_removed_id);
    this.connected = false;
  }
  destroy() {
    this.disconnect();
  }
}

const mountsMonitor = new MountsMonitor();
mountsMonitor.connect();

const colors = {
  "memory-program-color": '#00b35b',
  "memory-buffer-color": '#00ff82',
  "memory-cache-color": '#aaf5d0',
  "net-down-color": '#fce94f',
  "net-up-color": '#fb74fb',
  "net-downerrors-color": '#ff6e00',
  "net-uperrors-color": '#e0006e',
  "net-collisions-color": '#ff0000',
  "cpu-user-color": '#0072b3',
  "cpu-system-color": '#0092e6',
  "cpu-nice-color": '#00a3ff',
  "cpu-iowait-color": '#002f3d',
  "cpu-other-color": '#001d26',
  "disk-read-color": '#c65000',
  "disk-write-color": '#ff6700'
};

class ElementBase {
  constructor(properties) {
    this.actor = new St.BoxLayout({ reactive: true });
    this.actor._delegate = this;
    this.elt = '';
    this.color_name = [];

    Object.assign(this, properties);

    this.vals = [];

    this.colors = [];
    for (let color in this.color_name) {
      let name = this.elt + '-' + this.color_name[color] + '-color';
      let clutterColor = color_from_string(colors[name]);
      this.colors.push(clutterColor);
    }

    this.actor.visible = true;
    this.interval = 2000;
    this.timeout = mainloop.timeout_add(
      this.interval,
      this.update.bind(this),
      GLib.PRIORITY_DEFAULT_IDLE
    );

    this.chart = new Chart(40, main.layoutManager.panelBox.height - 4, this);
    this.actor.add_actor(this.chart.actor);
    this.chart.actor.visible = true;
  }

  update() {
    if (!this.actor.visible) return false;
    this.refresh();
    this._apply();
    this.chart.update();
    return true;
  }

  destroy() {
    if (this.timeout) {
      mainloop.source_remove(this.timeout);
      this.timeout = null;
    }
  }
}

class Cpu extends ElementBase {
  constructor(cpuid) {
    super({
      elt: 'cpu',
      color_name: ['user', 'system', 'nice', 'iowait', 'other'],
      cpuid: -1 // cpuid is -1 when all cores are displayed in the same graph
    });
    this.max = 100;

    this.cpuid = cpuid;
    this.gtop = new GTop.glibtop_cpu();
    this.last = [0, 0, 0, 0, 0];
    this.current = [0, 0, 0, 0, 0];
    try {
      this.total_cores = GTop.glibtop_get_sysinfo().ncpu;
      if (cpuid === -1)
        this.max *= this.total_cores;
    } catch (e) {
      this.total_cores = 1;
      global.logError(e)
    }
    this.last_total = 0;
    this.usage = [0, 0, 0, 1, 0];
    this.update();
  }

  refresh() {
    GTop.glibtop_get_cpu(this.gtop);
    // display global cpu usage on 1 graph
    if (this.cpuid === -1) {
      this.current[0] = this.gtop.user;
      this.current[1] = this.gtop.sys;
      this.current[2] = this.gtop.nice;
      this.current[3] = this.gtop.idle;
      this.current[4] = this.gtop.iowait;
      let delta = (this.gtop.total - this.last_total) / (100 * this.total_cores);

      if (delta > 0) {
        for (let i = 0; i < 5; i++) {
          this.usage[i] = Math.round((this.current[i] - this.last[i]) / delta);
          this.last[i] = this.current[i];
        }
        this.last_total = this.gtop.total;
      } else if (delta < 0) {
        this.last = [0, 0, 0, 0, 0];
        this.current = [0, 0, 0, 0, 0];
        this.last_total = 0;
        this.usage = [0, 0, 0, 1, 0];
      }
    } else {
      // display per cpu data
      this.current[0] = this.gtop.xcpu_user[this.cpuid];
      this.current[1] = this.gtop.xcpu_sys[this.cpuid];
      this.current[2] = this.gtop.xcpu_nice[this.cpuid];
      this.current[3] = this.gtop.xcpu_idle[this.cpuid];
      this.current[4] = this.gtop.xcpu_iowait[this.cpuid];
      let delta = (this.gtop.xcpu_total[this.cpuid] - this.last_total) / 100;

      if (delta > 0) {
        for (let i = 0; i < 5; i++) {
          this.usage[i] = Math.round((this.current[i] - this.last[i]) / delta);
          this.last[i] = this.current[i];
        }
        this.last_total = this.gtop.xcpu_total[this.cpuid];
      } else if (delta < 0) {
        this.last = [0, 0, 0, 0, 0];
        this.current = [0, 0, 0, 0, 0];
        this.last_total = 0;
        this.usage = [0, 0, 0, 1, 0];
      }
    }
  }

  _apply() {
    let other = 100;
    for (let i = 0; i < this.usage.length; i++)
      other -= this.usage[i];
    // Not to be confusing
    other = Math.max(0, other);
    this.vals = [
      this.usage[0], this.usage[1],
      this.usage[2], this.usage[4], other
    ];
  }
}

class Disk extends ElementBase {
  constructor() {
    super({
      elt: 'disk',
      color_name: ['read', 'write']
    });
    this.mounts = mountsMonitor.get_mounts();
    mountsMonitor.add_listener(this.update_mounts.bind(this));
    this.last = [0, 0];
    this.usage = [0, 0];
    this.last_time = 0;
    this.update();
  }

  update_mounts(mounts) {
    this.mounts = mounts;
  }

  refresh() {
    let accum = [0, 0];

    let file = Gio.file_new_for_path('/proc/diskstats');
    file.load_contents_async(null, (source, result) => {
      let as_r = source.load_contents_finish(result);
      let lines = parse_bytearray(as_r[1]).toString().split('\n');

      for (let i = 0; i < lines.length; i++) {
        let line = lines[i];
        let entry = line.trim().split(/[\s]+/);
        if (typeof (entry[1]) === 'undefined')
          break;
        accum[0] += parseInt(entry[5]);
        accum[1] += parseInt(entry[9]);
      }

      let time = GLib.get_monotonic_time() / 1000;
      let delta = (time - this.last_time) / 1000;
      if (delta > 0) {
        for (let i = 0; i < 2; i++) {
          this.usage[i] = ((accum[i] - this.last[i]) / delta / 1024 / 8);
          this.last[i] = accum[i];
        }
      }
      this.last_time = time;
    });
  }

  _apply() {
    this.vals = this.usage.slice();
    for (let i = 0; i < 2; i++) {
      if (this.usage[i] < 10) {
        this.usage[i] = Math.round(10 * this.usage[i]) / 10;
      } else {
        this.usage[i] = Math.round(this.usage[i]);
      }
    }
  }
}

class Mem extends ElementBase {
  constructor() {
    super({
      elt: 'memory',
      color_name: ['program', 'buffer', 'cache']
    });
    this.max = 1;

    this.gtop = new GTop.glibtop_mem();
    this.mem = [0, 0, 0];

    GTop.glibtop_get_mem(this.gtop);
    this.total = Math.round(this.gtop.total / 1024 / 1024);
    let threshold = 4 * 1024; // In MiB
    this.useGiB = false;
    this._unitConversion = 1024 * 1024;
    this._decimals = 100;
    if (this.total > threshold) {
      this.useGiB = true;
      this._unitConversion *= 1024 / this._decimals;
    }

    this.update();
  }
  refresh() {
    GTop.glibtop_get_mem(this.gtop);
    if (this.useGiB) {
      this.mem[0] = Math.round(this.gtop.user / this._unitConversion);
      this.mem[0] /= this._decimals;
      this.mem[1] = Math.round(this.gtop.buffer / this._unitConversion);
      this.mem[1] /= this._decimals;
      this.mem[2] = Math.round(this.gtop.cached / this._unitConversion);
      this.mem[2] /= this._decimals;
      this.total = Math.round(this.gtop.total / this._unitConversion);
      this.total /= this._decimals;
    } else {
      this.mem[0] = Math.round(this.gtop.user / this._unitConversion);
      this.mem[1] = Math.round(this.gtop.buffer / this._unitConversion);
      this.mem[2] = Math.round(this.gtop.cached / this._unitConversion);
      this.total = Math.round(this.gtop.total / this._unitConversion);
    }
  }
  _pad(number) {
    if (this.useGiB) {
      if (number < 1) {
        // examples: 0.01, 0.10, 0.88
        return number.toLocaleString('en-US', {minimumFractionDigits: 2, maximumFractionDigits: 2});
      }
      // examples: 5.85, 16.0, 128
      return number.toLocaleString('en-US', {minimumSignificantDigits: 3, maximumSignificantDigits: 3});
    }

    return number.toLocaleString('en-US');
  }
  _apply() {
    if (this.total === 0) {
      this.vals = [0, 0, 0];
    } else {
      for (let i = 0; i < 3; i++)
        this.vals[i] = this.mem[i] / this.total;
    }
  }
}

class Net extends ElementBase {
  constructor() {
    super({
      elt: 'net',
      color_name: ['down', 'downerrors', 'up', 'uperrors', 'collisions']
    });
    this.speed_in_bits = false;
    this.ifs = [];
    this.client = NM.Client.new(null);
    this.update_iface_list();

    if (!this.ifs.length) {
      let net_lines = Shell.get_file_contents_utf8_sync('/proc/net/dev').split('\n');
      for (let i = 2; i < net_lines.length - 1; i++) {
        const ifc = net_lines[i].replace(/^\s+/g, '').split(':')[0];
        if (Shell.get_file_contents_utf8_sync('/sys/class/net/' + ifc + '/operstate')
          .replace(/\s/g, '') === 'up' &&
          ifc.indexOf('br') < 0 &&
          ifc.indexOf('lo') < 0) {
          this.ifs.push(ifc);
        }
      }
    }
    this.gtop = new GTop.glibtop_netload();
    this.last = [0, 0, 0, 0, 0];
    this.usage = [0, 0, 0, 0, 0];
    this.last_time = 0;
    this.speed_in_bits = false;
    try {
      let iface_list = this.client.get_devices();
      this.NMsigID = [];
      for (let j = 0; j < iface_list.length; j++) {
        this.NMsigID[j] = iface_list[j].connect('state-changed', this.update_iface_list.bind(this));
      }
    } catch (e) {
      global.logError('Please install Network Manager Gobject Introspection Bindings: ' + e);
    }
    this.update();
  }
  update_iface_list() {
    try {
      this.ifs = [];
      let iface_list = this.client.get_devices();
      for (let j = 0; j < iface_list.length; j++) {
        if (iface_list[j].state === NM.DeviceState.ACTIVATED)
          this.ifs.push(iface_list[j].get_ip_iface() || iface_list[j].get_iface());
      }
    } catch (e) {
      global.logError('Please install Network Manager Gobject Introspection Bindings');
    }
  }
  refresh() {
    let accum = [0, 0, 0, 0, 0];

    for (let ifn in this.ifs) {
      GTop.glibtop_get_netload(this.gtop, this.ifs[ifn]);
      accum[0] += this.gtop.bytes_in;
      accum[1] += this.gtop.errors_in;
      accum[2] += this.gtop.bytes_out;
      accum[3] += this.gtop.errors_out;
      accum[4] += this.gtop.collisions;
    }

    let time = GLib.get_monotonic_time() * 0.001024;
    let delta = time - this.last_time;
    if (delta > 0) {
      for (let i = 0; i < 5; i++) {
        this.usage[i] = Math.round((accum[i] - this.last[i]) / delta);
        this.last[i] = accum[i];
        this.vals[i] = this.usage[i];
      }
    }
    this.last_time = time;
  }

  // pad a string with leading spaces
  _pad(number, length) {
    var str = '' + number;
    while (str.length < length)
      str = ' ' + str;
    return str;
  }

  _apply() {}
}

const box = new St.BoxLayout({ style: 'spacing: 4px; padding: 0 8px 0 4px;' });
main.panel.statusArea.aggregateMenu._indicators?.insert_child_at_index(box, 0);
const elts = [new Cpu(-1), new Mem(), new Disk(), new Net()];
for (let elt in elts)
  box.add_actor(elts[elt].actor);

})()
