printf '\nen_US.UTF-8 UTF-8\n' >> /etc/locale.gen
locale-gen
printf 'LANG=en_US.UTF-8\n' > /etc/locale.conf

pacman -S grub intel-ucode amd-ucode linux linux-firmware \
  btrfs-progs e2fsprogs dosfstools udisks2 networkmanager pulseaudio-alsa \
  nano man-db unzip ttf-hack noto-fonts materia-gtk-theme \
  gnome-shell gdm gvfs gnome-terminal

printf '\nGRUB_TIMEOUT=0\nGRUB_DISABLE_OS_PROBER=true\n' >> /etc/default/grub
printf '\nset superusers=""\n' >> /etc/grub.d/40_custom
printf '\nCLASS="--class gnu-linux --class gnu --class os --unrestricted"\n' >
  /etc/grub.d/10_linux
grub-mkconfig -o /boot/grub/grub.cfg
grub-mkstandalone -O x86_64-efi -o '/boot/efi/EFI/BOOT/BOOTX64.EFI' \
  'boot/grub/grub.cfg=/boot/grub/grub.cfg'
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

systemctl enable systemd-timesyncd
systemctl enable NetworkManager
systemctl enable NetworkManager-dispatcher
systemctl enable gdm

# to customize dconf default values:
mkdir -p /etc/dconf/profile
echo 'user-db:user
system-db:local
' > /etc/dconf/profile/user

mkdir -p /etc/dconf/db/local.d
echo "[org/gnome/system/location]
enabled=true
[org/gnome/desktop/datetime]
automatic-timezone=true
[org/gnome/desktop/notifications]
show-banners=false
show-in-lock-screen=false
[org/gnome/desktop/background]
primary-color='#222222'
secondary-color='#222222'
[org/gnome/desktop/interface]
overlay-scrolling=false
document-font-name='sans 10.5'
font-name='sans 10.5'
monospace-font-name='monospace 10.5'
gtk-theme='Materia-light-compact'
cursor-blink-timeout=1000
enable-hot-corners=false
[org/gnome/mutter]
center-new-windows=true
[org/gnome/desktop/wm/preferences]
button-layout=''
[org/gnome/desktop/wm/keybindings]
close=['<Alt>Escape']
toggle-maximized=['<Alt><Shift>Space']
activate-window-menu=['']
[org/gnome/shell/keybindings]
toggle-application-view=['<Alt>Space', '<Super>a']
[org/gnome/shell]
disable-extension-version-validation=true
enabled-extensions=['gnome-shell-improved']
" > /etc/dconf/db/local.d/00-mykeyfile
dconf update

mkdir -p /usr/local/share/gnome-shell/extensions/gnome-shell-improved/
echo '{
  "uuid": "gnome-shell-improved",
  "name": "GnomeShellImproved",
  "description": "GnomeShell improved",
  "shell-version": []
}' > /usr/local/share/gnome-shell/extensions/gnome-shell-improved/metadata.json
echo 'stage {
  font-family: sans;
  font-size: 10.5pt;
  font-weight: normal;
  color: #ffffff;
}
#panel {
  height: 18px;
}
#panel .panel-button {
  -natural-hpadding: 4px;
  -minimum-hpadding: 4px;
  margin: 1px 0px 0px 0px;
  font-family: monospace;
  font-size: 10.5pt;
  font-weight: normal;
  color: #ffffff;
}
.panel-status-indicators-box {
  spacing: 8px;
}
.window-button-icon {
  height: 16px;
  padding: 0px 2px 0 0;
}
' > /usr/local/share/gnome-shell/extensions/gnome-shell-improved/style.css
cp ./extension.js /usr/local/share/gnome-shell/extensions/gnome-shell-improved/

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
echo '[Settings]
gtk-theme-name = Materia-light-compact
gtk-font-name = Sans
' > /etc/skel/.config/gtk-3.0/settings.ini
mkdir -p /etc/skel/.config/gtk-4.0
cp ./gtk.css /etc/skel/.config/gtk-4.0/
echo '[Settings]
gtk-theme-name = Materia-light-compact
gtk-font-name = Sans
' > /etc/skel/.config/gtk-4.0/settings.ini

echo '
PS1="\[$(tput setaf 1)\]\w >\[$(tput sgr0)\] "
unset HISTFILE
' >> /etc/skel/.bashrc

echo '
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
  exec sway
fi
' >> /etc/skel/.bash_profile

mkdir -p /etc/skel/.config/sway
cp ./sway /etc/skel/.config/sway/config
cp ./swayshell.js /usr/local/share/

printf '[Desktop Entry]\nNoDisplay=true' |
tee /etc/skel/.local/share/applications/{avahi-discover,bssh,bvnc,qv4l2,qvidcap,lstopo}.desktop >
  /dev/null

mkdir -p /etc/skel/.config/alacritty
echo 'window:
  dynamic_padding: true
font:
  size: 10.5
key_bindings:
  - { key: N, mods: Control, command: { program: "alacritty" } }
  - { key: Enter, mods: Alt, command: { program: "alacritty" } }
' > /etc/skel/.config/alacritty/alacritty.yml

useradd -m -G wheel user1
passwd user1
passwd
