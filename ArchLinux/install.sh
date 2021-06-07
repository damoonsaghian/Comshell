printf '\nen_US.UTF-8 UTF-8\n' >> /etc/locale.gen
locale-gen
printf 'LANG=en_US.UTF-8\n' > /etc/locale.conf

pacman -S grub intel-ucode amd-ucode linux linux-firmware \
  btrfs-progs e2fsprogs dosfstools udisks2 networkmanager pulseaudio-alsa alsa-utils \
  nano man-db unzip ttf-hack noto-fonts materia-gtk-theme \
  gnome-shell gdm gvfs gst-plugins-{base,good,bad} gst-libav gnome-terminal

printf '\nGRUB_TIMEOUT=0\nGRUB_DISABLE_OS_PROBER=true\n' >> /etc/default/grub
# disable menu editing and other admin operations in Grub (to prevent root access using "rd.break" boot option):
printf 'cat <<EOF\nset superusers=""\nset menuentry_id_option="--unrestricted $menuentry_id_option"\nEOF' >
  /etc/grub.d/01_users
chmod +x /etc/grub.d/01_users
grub-mkconfig -o /boot/grub/grub.cfg
grub-mkstandalone -O x86_64-efi -o '/boot/efi/EFI/BOOT/BOOTX64.EFI' 'boot/grub/grub.cfg=/boot/grub/grub.cfg'
# automatically update Grub every time "grub" package is upgraded:
mkdir -p /etc/pacman.d/hooks
echo '[Trigger]
Type = Package
Operation = Upgrade
Target = grub
[Action]
Description = Updating grub
When = PostTransaction
Exec = /usr/bin/grub-mkstandalone -O x86_64-efi -o "/boot/efi/EFI/BOOT/BOOTX64.EFI" "boot/grub/grub.cfg=/boot/grub/grub.cfg"
' > /etc/pacman.d/hooks/100-grub.hook

# package management and system updater service;
# create a base directory;
# create snapshots of "usr", and mount it in the base directory;
# for the rest of system root directories make symlinks in the base directory;
# chroot and upgrade;
# generate fstab
#
# "https://www.techrapid.uk/2017/04/automatically-update-arch-linux-with-systemd.html"
# "https://wiki.archlinux.org/index.php/Systemd/Timers"

systemctl enable systemd-timesyncd
systemctl enable NetworkManager
systemctl enable NetworkManager-dispatcher
systemctl enable gdm

amixer sset Master unmute
amixer sset Master 0dB
amixer sset Capture cap
alsactl store

# to customize dconf default values:
mkdir -p /etc/dconf/profile
echo 'user-db:user
system-db:local
' > /etc/dconf/profile/user

mkdir -p /etc/dconf/db/local.d
echo "[org/gnome/system/location]
enabled=true
[org/gnome/desktop/notifications]
show-banners=false
show-in-lock-screen=false
[org/gnome/desktop/interface]
clock-format='12h'
overlay-scrolling=false
document-font-name='sans 10.5'
font-name='sans 10.5'
monospace-font-name='monospace 10.5'
gtk-theme='Materia-light-compact'
cursor-blink-timeout=1000
enable-hot-corners=false
[org/gnome/desktop/session]
idle-delay=600
[org/gnome/desktop/wm/preferences]
button-layout=''
[org/gnome/desktop/background]
primary-color='#282828'
secondary-color='#282828'
[org/gnome/desktop/wm/keybindings]
cycle-group=['<Alt>Above_Tab', '<Alt>a', '<Alt>Comma']
toggle-maximized=['<Alt><Shift>Space']
close=['<Alt>Escape']
cycle-windows=['']
cycle-windows-backward=['']
activate-window-menu=['']
[org/gnome/shell/keybindings]
toggle-application-view=['<Alt>Space']
[org/gnome/shell]
disable-extension-version-validation=true
enabled-extensions=['gnome-shell-improved']
favorite-apps=[]
[org/gnome/terminal/legacy]
default-show-menubar=false
headerbar=just false
new-terminal-mode='tab'
[org/gnome/terminal/legacy/keybindings]
close-tab='<Control>w'
close-window='<Control>q'
new-tab='<Control>t'
[org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9]
scrollbar-policy='never'
default-size-columns=130
default-size-rows=50
" > /etc/dconf/db/local.d/00-mykeyfile
dconf update

mkdir -p /usr/local/share/gnome-shell/extensions/gnome-shell-improved/
cp ./extension.js /usr/local/share/gnome-shell/extensions/gnome-shell-improved/
echo '{
  "uuid": "gnome-shell-improved",
  "name": "GnomeShellImproved",
  "description": "GnomeShell improved",
  "shell-version": []
}' > /usr/local/share/gnome-shell/extensions/gnome-shell-improved/metadata.json
echo 'stage {
  font-family: sans;
  font-size: 10.5pt;
}
#panel {
  background-color: transparent;
}
#panel .panel-button {
  font-family: sans;
  font-size: 10.5pt;
  font-weight: normal;
  color: #ffffff;
}
#panel #panelActivities {
  -natural-hpadding: 0;
}
' > /usr/local/share/gnome-shell/extensions/gnome-shell-improved/style.css

mkdir -p /etc/skel/.local/share/applications
printf '[Desktop Entry]\nNoDisplay=true' |
tee /etc/skel/.local/share/applications/\
{avahi-discover,bssh,bvnc,qv4l2,qvidcap,lstopo,nm-connection-editor}.desktop > /dev/null
echo '[Desktop Entry]
Name=Extensions
Exec=/usr/bin/gnome-extensions-app
Type=Application
DBusActivatable=true
Icon=org.gnome.Extensions
NoDisplay=true
' > /etc/skel/.local/share/applications/org.gnome.Extensions.desktop

echo '<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig>
  <selectfont>
    <rejectfont>
      <pattern><patelt name="family"><string>NotoNastaliqUrdu</string></patelt></pattern>
      <pattern><patelt name="family"><string>NotoKufiArabic</string></patelt></pattern>
      <pattern><patelt name="family"><string>NotoNaskhArabic</string></patelt></pattern>
      <pattern><patelt name="family"><string>NotoNaskhArabicUI</string></patelt></pattern>
    </rejectfont>
  </selectfont>
  <alias>
    <family>serif</family>
    <prefer><family>NotoSerif</family></prefer>
  </alias>
  <alias>
    <family>sans-serif</family>
    <prefer><family>NotoSans</family></prefer>
  </alias>
  <alias>
    <family>sans</family>
    <prefer><family>NotoSans</family></prefer>
  </alias>
  <alias>
    <family>monospace</family>
    <prefer><family>Hack</family></prefer>
  </alias>
</fontconfig>
' > /etc/fonts/local.conf

mkdir -p /etc/skel/.config/gtk-3.0
cp ./gtk.css /etc/skel/.config/gtk-3.0/
mkdir -p /etc/skel/.config/gtk-4.0
cp ./gtk.css /etc/skel/.config/gtk-4.0/

echo '
PS1="\[$(tput setaf 1)\]\w >\[$(tput sgr0)\] "
unset HISTFILE
' >> /etc/skel/.bashrc

passwd
useradd -m -G wheel user1
passwd user1
