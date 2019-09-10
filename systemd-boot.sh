bootctl --path=/boot install
mkdir -p /boot/loader/entries

echo -e "default arch+\neditor no" > /boot/loader/loader.conf

echo "title Arch Linux
linux /vmlinuz-linux
initrd /intel-ucode.img
initrd /initramfs-linux.img
options root=UUID=" > /boot/loader/entries/arch.conf
blkid -s UUID -o value $1 >> /boot/loader/entries/arch.conf
echo -e " rw\n" >> /boot/loader/entries/arch.conf

echo "title Arch Linux (fallback)
linux /vmlinuz-linux
initrd /intel-ucode.img
initrd /initramfs-linux-fallback.img
options root=UUID=" > /boot/loader/entries/arch-fallback.conf
blkid -s UUID -o value $1 >> /boot/loader/entries/arch-fallback.conf
echo -e " rw\n" >> /boot/loader/entries/arch-fallback.conf

# automatically update "systemd-boot" every time "systemd" package is upgraded:
mkdir -p /etc/pacman.d/hooks
echo "[Trigger]
Type = Package
Operation = Upgrade
Target = systemd

[Action]
Description = Updating systemd-boot
When = PostTransaction
Exec = /usr/bin/bootctl update
" > /etc/pacman.d/hooks/100-systemd-boot.hook
