timedatectl set-ntp true

printf "label: gpt\n,260MiB,U,*\n;" | sfdisk /dev/"$1"
mkfs.fat -F32 /dev/"$1"1; mkfs.btrfs /dev/"$1"2
mkdir -p /mnt/boot/efi
mount /dev/"$1"1 /mnt/boot/efi

# create and mount subvolumes for: / etc home root opt usr/local srv tmp var
#mount /dev/"$1"2/subvol1 /mnt
#mkdir /mnt/etc
#mkdir /mnt/home
#mkdir /mnt/root
#mkdir /mnt/opt
#mkdir -p /mnt/usr/local
#mkdir /mnt/srv
#mkdir /mnt/tmp
#mkdir /mnt/var
#mount /dev/"$1"2/subvol1 /mnt/etc
#mount /dev/"$1"2/subvol1 /mnt/home
#mount /dev/"$1"2/subvol1 /mnt/root
#mount /dev/"$1"2/subvol1 /mnt/opt
#mount /dev/"$1"2/subvol1 /mnt/usr/local
#mount /dev/"$1"2/subvol1 /mnt/srv
#mount /dev/"$1"2/subvol1 /mnt/tmp
#mount /dev/"$1"2/subvol1 /mnt/var

pacstrap /mnt base
genfstab -U /mnt >> /mnt/etc/fstab
