printf '\nen_US.UTF-8 UTF-8\n' >> /etc/locale.gen
locale-gen
printf 'LANG=en_US.UTF-8\n' > /etc/locale.conf

pacman -S grub intel-ucode amd-ucode linux linux-firmware \
  btrfs-progs e2fsprogs dosfstools unzip nano man-db pulseaudio-alsa networkmanager \
  ttf-hack noto-fonts materia-gtk-theme gvfs \
  lightdm-gtk-greeter xorg-server light-locker gnome-shell

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
Exec = /usr/bin/grub-mkstandalone -O x86_64-efi -o \"/boot/efi/EFI/BOOT/BOOTX64.EFI\" \"boot/grub/grub.cfg=/boot/grub/grub.cfg\"
' > /etc/pacman.d/hooks/100-grub.hook

# "https://www.techrapid.uk/2017/04/automatically-update-arch-linux-with-systemd.html"
# "https://wiki.archlinux.org/index.php/Systemd/Timers"
# download updates as scheduled;
# put "reboot to update" in notifications;
# before reboot/poweroff install the updates, then delete the notification;
# https://kubic.opensuse.org/blog/2018-04-04-transactionalupdates/
# https://github.com/openSUSE/transactional-update

systemctl enable systemd-timesyncd
systemctl enable NetworkManager
systemctl enable lightdm

mkdir -p /etc/lightdm/lightdm.conf.d/
echo '
[Seat:*]
allow-guest=false
user-session=gnome
autologin-session=gnome
' > /etc/lightdm/lightdm.conf.d/50-myconfig.conf

mkdir -p /etc/lightdm/lightdm-gtk-greeter.conf.d/
echo '
[greeter]
hide-user-image = true
panel-position = bottom
clock-format = %F %a %p %I:%M
indicators = ~spacer;~power;~clock
' > /etc/lightdm/lightdm-gtk-greeter.conf.d/50-myconfig.conf
# https://git.launchpad.net/lightdm-gtk-greeter/tree/data/sample-lightdm-gtk-greeter.css

mkdir -p /etc/skel/.config/autostart
echo '[Desktop Entry]
Type=Application
Name=Screen Locker
Exec=light-locker
NoDisplay=true
' > /etc/skel/.config/autostart/light-locker.desktop

# https://access.redhat.com/documentation/en-us/red_hat_enterprise_linux/7/html/desktop_migration_and_administration_guide/custom-default-values-system-settings
echo '
' > /usr/share/glib-2.0/schemas/19_mysettings.gschema.override
glib-compile-schemas /usr/share/glib-2.0/schemas

mkdir -p /etc/dconf/db/local.d/locks
echo '
' > /etc/dconf/db/local.d/locks/00_mylocks
dconf update

mkdir -p /usr/local/share/gnome-shell/extensions/gnome-shell-improved/
echo '{
  "uuid": "gnome-shell-improved"
}' > /usr/local/share/gnome-shell/extensions/gnome-shell-improved/metadata.json
cp ./extension.js /usr/local/share/gnome-shell/extensions/gnome-shell-improved/

echo '<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig>
  <selectfont>
    <rejectfont>
      <pattern><patelt name="family" ><string>NotoNastaliqUrdu</string></patelt></pattern>
      <pattern><patelt name="family" ><string>NotoKufiArabic</string></patelt></pattern>
      <pattern><patelt name="family" ><string>NotoNaskhArabic</string></patelt></pattern>
      <pattern><patelt name="family" ><string>NotoNaskhArabicUI</string></patelt></pattern>
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

echo "[org/gnome/desktop/interface]
gtk-theme = 'Materia-light-compact'
font-name = 'Sans'
" > /etc/dconf/db/local.d/1
dconf update

mkdir -p /etc/skel/.config/gtk-3.0
cp ./gtk.css /etc/skel/.config/gtk-3.0/
mkdir -p /etc/skel/.config/gtk-4.0
cp ./gtk.css /etc/skel/.config/gtk-4.0/

echo '
PS1="\[$(tput setab 6)\]\[$(tput setaf 0)\]\w\[$(tput sgr0)\]\[$(tput setaf 6)\]\[$(tput sgr0)\] "
unset HISTFILE
' >> /etc/skel/.bashrc

useradd -m -G wheel user1
passwd user1
passwd

rm ./arch-install.sh ./extension.js ./gtk.css
