pacman -S grub intel-ucode amd-ucode linux linux-firmware \
  btrfs-progs e2fsprogs dosfstools udisks2 pulseaudio-alsa networkmanager \
  sudo nano man-db unzip \
  sway sakura gvfs materia-gtk-theme ttf-hack noto-fonts

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
fi' > /etc/NetworkManager/dispatcher.d/09-timezone
chmod 755 /etc/NetworkManager/dispatcher.d/09-timezone

systemctl enable systemd-timesyncd
systemctl enable NetworkManager

# https://www.techrapid.uk/2017/04/automatically-update-arch-linux-with-systemd.html
# https://wiki.archlinux.org/index.php/Systemd/Timers

echo '
Defaults requiretty
%wheel ALL=(ALL) ALL
' >> /etc/sudoers

echo '
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
  sway&
fi
' >> /etc/skel/.bash_profile

echo '
PS1="\[$(tput setab 6)\]\[$(tput setaf 0)\]\w >\[$(tput sgr0)\] "
unset HISTFILE
alias mount="udisksctl mount -b"
alias umount="udisksctl unmount -b"
' >> /etc/skel/.bashrc

mkdir -p /etc/skel/.config/sway
cp sway /etc/skel/.config/sway/config

mkdir -p /etc/skel/.config/sakura
echo '[sakura]
colorset1_fore=rgb(255,255,255)
colorset1_back=rgba(55,55,55,0.98)
colorset1_curs=rgb(255,255,255)
palette=solarized_light
font=Monospace 10.5
blinking_cursor=Yes
add_tab_accelerator=4
del_tab_accelerator=4
search_accelerator=4
prev_tab_key=Prior
next_tab_key=Next
' > /etc/skel/.config/sakura/sakura.conf

useradd -m -G wheel user1
passwd user1
passwd

# automatic login:
mkdir /etc/systemd/system/getty@tty1.service.d
echo '[Service]
ExecStart=
ExecStart=-/usr/bin/agetty --autologin user1 --noclear %I $TERM
' > /etc/systemd/system/getty@tty1.service.d/override.conf

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
