pacman -S grub intel-ucode amd-ucode linux linux-firmware \
  btrfs-progs e2fsprogs dosfstools udisks2 pulseaudio-alsa networkmanager \
  nano man-db unzip gdm alacrity gvfs materia-gtk-theme ttf-hack noto-fonts

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

printf '\nen_US.UTF-8 UTF-8\n' >> /etc/locale.gen
locale-gen
printf 'LANG=en_US.UTF-8\n' > /etc/locale.conf

# automatic time_zone:
#!/bin/sh
echo 'if [ "$2" = "connectivity-change" ] && [ -z "$VPN_IP_IFACE" ]; then
  timedatectl set-timezone "$(curl --fail http://ip-api.com/line/?fields=timezone)"
fi
' > /etc/NetworkManager/dispatcher.d/09-timezone
chmod 755 /etc/NetworkManager/dispatcher.d/09-timezone

systemctl enable systemd-timesyncd
systemctl enable NetworkManager

echo '
PS1="\[$(tput setab 6)\]\[$(tput setaf 0)\]\w\[$(tput sgr0)\]\[$(tput setaf 6)\]\[$(tput sgr0)\] "
unset HISTFILE
alias mount="udisksctl mount -b"
alias umount="udisksctl unmount -b"
alias reboot="( swaymsg [title=.] kill; sleep 0.5; systemctl reboot ) & disown"
alias poweroff="( swaymsg [title=.] kill; sleep 0.5; systemctl poweroff ) & disown"
' >> /etc/skel/.bashrc

#mkdir -p /etc/skel/.config/alacrity
#echo '
#' > /etc/skel/.config/alacrity/alacrity.conf

useradd -m -G wheel user1
passwd user1
passwd

systemctl enable gdm
# automatic login:
echo '[daemon]
AutomaticLogin=user1
AutomaticLoginEnable=True
' > /etc/gdm/custom.conf

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

#echo "[org/gnome/desktop/interface]
#gtk-theme = 'Materia-light-compact'
#font-name = 'Sans'
#[org/gnome/desktop/screensaver]
#lock-enabled = 'false'
#" > /etc/dconf/db/local.d/1
#dconf update

# https://access.redhat.com/documentation/en-us/red_hat_enterprise_linux/7/html/desktop_migration_and_administration_guide/custom-default-values-system-settings
# https://wiki.gnome.org/Projects/GnomeShell/Extensions
# https://wiki.gnome.org/Projects/GnomeShell/Extensions/Writing
mkdir -p /usr/local/share/gnome-shell/extensions/gnome-shell-improved/
echo '{
  "uuid": "gnome-shell-improved"
}' > /usr/local/share/gnome-shell/extensions/gnome-shell-improved/metadata.json
cp ./extension.js /usr/local/share/gnome-shell/extensions/gnome-shell-improved/
