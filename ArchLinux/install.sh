timedatectl set-ntp true

printf "label: gpt\n,260MiB,U,*\n;" | sfdisk /dev/"$1"
mkfs.fat -F32 /dev/"$1"1; mkfs.btrfs /dev/"$1"2

mount /dev/"$1"2 /mnt
# create subvolumes for "/ etc home root opt usr/local srv tmp var":
btrfs subvolume create /mnt/0
btrfs subvolume create /mnt/etc
btrfs subvolume create /mnt/home
btrfs subvoulme create /mnt/root
btrfs subvolume create /mnt/opt
btrfs subvolume create /mnt/local
btrfs subvolume create /mnt/srv
btrfs subvolume create /mnt/tmp
btrfs subvolume create /mnt/var
umount /mnt

mount /dev/"$1"2 /mnt -o subvol=0
mkdir /mnt/etc
mkdir /mnt/home
mkdir /mnt/root
mkdir /mnt/opt
mkdir -p /mnt/0/usr/local
mkdir /mnt/srv
mkdir /mnt/tmp
mkdir /mnt/var
mkdir /mnt/subvols
mount -o subvol=etc /dev/"$1"2 /mnt/etc
mount -o subvol=home /dev/"$1"2 /mnt/home
mount -o subvol=root /dev/"$1"2 /mnt/root
mount -o subvol=opt /dev/"$1"2 /mnt/opt
mount -o subvol=local /dev/"$1"2 /mnt/usr/local
mount -o subvol=srv /dev/"$1"2 /mnt/srv
mount -o subvol=tmp /dev/"$1"2 /mnt/tmp
mount -o subvol=var /dev/"$1"2 /mnt/var
mount /dev/"$1"2 /mnt/subvols

## "arc" service does automatic updates, and accepts add and remove requests from wheel users;
#
## if "/" is "/subvols/0", if "/subvols/1" is older, delete it,
##   and create a snapshot of "/subvols/0" in "/subvols/1";
## if "/" is "/subvols/1", if "/subvols/0" is older, delete it,
##   and create a snapshot of "/subvols/1" in "/subvols/0";
#btrfs subvolume snapshot /subvols/0 /subvols/1
#
#bind_mount "etc home root opt usr/local srv tmp var";
#
#arch-chroot /subvols/1 << EOF
#pacman -Syu
## in the case of add requests, install packages;
#grub-mkconfig -o /boot/grub/grub.cfg
#btrfs subvolume delete /subvols/0
#EOF
#
## "https://www.techrapid.uk/2017/04/automatically-update-arch-linux-with-systemd.html"
## "https://wiki.archlinux.org/index.php/Systemd/Timers"
#
## clear cache: pacman -Sc
## clear orphaned packages: pacman -Qttd
## remove package: pacman -Rns ...

mkdir -p /mnt/boot/efi
mount /dev/"$1"1 /mnt/boot/efi

pacstrap /mnt base
genfstab -U /mnt >> /mnt/etc/fstab

# disable copy_on_write for databases
# autodefrag mount options (disabled by default);
# ; btrfs fi defragment
# chattr (+C): disable COW for databases (Firefox, systemd /var/log/journal/, ...);
# ; lsattr

# to customize dconf default values:
mkdir -p /mnt/etc/dconf/profile
echo 'user-db:user
system-db:local
' > /mnt/etc/dconf/profile/user

mkdir -p /mnt/etc/dconf/db/local.d
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
switch-group=['<Alt>Above_Tab', '<Alt>a', '<Alt>Comma']
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
favorite-apps=['']
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
" > /mnt/etc/dconf/db/local.d/00-mykeyfile

mkdir -p /mnt/usr/local/share/gnome-shell/extensions/gnome-shell-improved/
cp ./extension.js /mnt/usr/local/share/gnome-shell/extensions/gnome-shell-improved/
echo '{
  "uuid": "gnome-shell-improved",
  "name": "GnomeShellImproved",
  "description": "GnomeShell improved",
  "shell-version": []
}' > /mnt/usr/local/share/gnome-shell/extensions/gnome-shell-improved/metadata.json
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
' > /mnt/usr/local/share/gnome-shell/extensions/gnome-shell-improved/style.css

mkdir -p /mnt/etc/skel/.local/share/applications
printf '[Desktop Entry]\nNoDisplay=true' |
tee /mnt/etc/skel/.local/share/applications/\
{avahi-discover,bssh,bvnc,qv4l2,qvidcap,lstopo,nm-connection-editor}.desktop > /dev/null
echo '[Desktop Entry]
Name=Extensions
Exec=/usr/bin/gnome-extensions-app
Type=Application
DBusActivatable=true
Icon=org.gnome.Extensions
NoDisplay=true
' > /mnt/etc/skel/.local/share/applications/org.gnome.Extensions.desktop

mkdir -p /mnt/etc/fonts
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
' > /mnt/etc/fonts/local.conf

mkdir -p /mnt/etc/skel/.config/gtk-3.0
cp ./gtk.css /mnt/etc/skel/.config/gtk-3.0/
mkdir -p /mnt/etc/skel/.config/gtk-4.0
cp ./gtk.css /mnt/etc/skel/.config/gtk-4.0/

echo '
PS1="\[$(tput setaf 1)\]\w >\[$(tput sgr0)\] "
unset HISTFILE
' >> /mnt/etc/skel/.bashrc

arch-chroot /mnt /usr/bin/sh << "EOF"
tzselect

printf '\nen_US.UTF-8 UTF-8\n' >> /etc/locale.gen
locale-gen
printf 'LANG=en_US.UTF-8\n' > /etc/locale.conf

pacman -S grub intel-ucode amd-ucode linux linux-firmware \
  btrfs-progs e2fsprogs dosfstools udisks2 networkmanager pulseaudio-alsa alsa-utils reflector \
  nano man-db unzip ttf-hack noto-fonts materia-gtk-theme \
  gnome-shell gdm gvfs gst-plugins-{base,good,bad} gst-libav gnome-terminal

printf '\nGRUB_TIMEOUT=0\nGRUB_DISABLE_OS_PROBER=true\n' >> /etc/default/grub
# disable menu editing and other admin operations in Grub:
printf '#! /bin/sh\nset superusers=""\nset menuentry_id_option="--unrestricted $menuentry_id_option"\n' >
  /etc/grub.d/09_user
chmod +x /etc/grub.d/09_user
grub-install --target=x86_64-efi --efi-directory=/boot/efi --removable
grub-mkconfig -o /boot/grub/grub.cfg
# automatically update Grub every time "grub" package is upgraded:
mkdir -p /etc/pacman.d/hooks
echo '[Trigger]
Type = Package
Operation = Upgrade
Target = grub
[Action]
Description = Updating grub
When = PostTransaction
Exec = grub-install --target=x86_64-efi --efi-directory=/boot/efi --removable
' > /etc/pacman.d/hooks/100-grub.hook

systemctl enable systemd-timesyncd
systemctl enable NetworkManager
systemctl enable NetworkManager-dispatcher
systemctl enable reflector
systemctl enable gdm

amixer sset Master unmute
amixer sset Master 0dB
amixer sset Capture cap
alsactl store

dconf update

passwd
useradd -m -G wheel user1
passwd user1
EOF

echo "installation completed; do you want to reboot?"
select yn in "yes" "no"; do
    case $yn in
        yes ) reboot; break;;
        no ) echo 'to reboot to the installed system, enter "reboot";';;
    esac
done
