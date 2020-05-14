# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
#
# Symbolic link this file with `configuration.nix`.
{ config, lib, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ./desktop-configuration.nix
    ./modules/syncthing.nix
  ];

  # -------------------------------------------------- Hardware configuration
  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usbhid" "sd_mod" ];

  # Module `kvm-intel` for virtualization with libvirtd/kvm. Load it
  # with the `nested=1` option to enable nested kvm (i.e., kvm VM in a
  # kvm VM). As a result, the following command should output Y:
  # > cat /sys/module/kvm_intel/parameters/nested
  # > Y
  # Add pci-stub and iommu for GPU passthrough, see
  # https://github.com/domenkozar/snabb-openstack-testing/tree/6310879eeb2b3b417dbe0e3b0ea5cd9f84aaa311
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModprobeConfig = "options kvm_intel nested=1";
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/e851ac1e-62a9-4b42-9f5a-2b1770c8a68d";
    fsType = "ext4";
  };

  fileSystems."/tmp" = {
    device = "tmpfs";
    fsType = "tmpfs";
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/255ee711-493c-45ff-b30e-62b917043345";
    fsType = "ext4";
  };

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 16;

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";

  # -------------------------------------------------- System configuration

  # Define your hostname.
  networking.hostName = "home-rfish";

  # Make the firewall unblock packet for syncthing (22000). Blocked
  # packets in dmesg/journalctl look like this:
  #
  # `rejected connection: IN=wlp2s0 OUT=
  # MAC=b8:8a:60:a6:bc:dd:d4:3d:7e:19:40:c4:86:dd
  # SRC=2a01:0e35:8a34:fa80:d63d:7eff:fe19:40c4
  # DST=2a01:0e35:8a34:fa80:bd34:49c2:56c9:3972 LEN=80 TC=0
  # HOPLIMIT=64 FLOWLBL=750773 PROTO=TCP SPT=42252 DPT=8888
  # WINDOW=28400 RES=0x00 SYN URGP=0`
  #
  # https://nixos.org/nixos/manual/index.html#sec-firewall
  networking.firewall.allowedTCPPorts = [ 22000 ];

  # Connect to my pi-hole
  #
  # A merge is not possible between `unbound.enable = true` from
  # configuration.nix and the following `unbound.enable = false`. The
  # `mkForce` ensures that the disabling of `unbound` takes precedence
  # over the definition in configuration.nix.
  # https://nixos.org/nixos/manual/#sec-modularity
  services.unbound.enable = lib.mkForce false;
  networking.nameservers = [ "192.168.1.253" ];

  environment.systemPackages = with pkgs; [
    # bluez
    ntfs3g
  ];

  # Bluetooth:
  # - Install bluez5
  # - Use bluetoothctl to pair and trust the keyboard
  # - Add udev rule to have the device active after reboot
  # See wiki.archlinux.org/index.php?title=Bluetooth&oldid=390632
  # See nixos.org/w/index.php?title=FAQ&oldid=24648#How_to_add_a_custom_udev_rule_in_nixos.3F
  hardware.bluetooth.enable = true;

  hardware.opengl.extraPackages = with pkgs; [ vaapiIntel vaapiVdpau ];
  services.xserver = {
    videoDrivers = [ "nvidia" ];

    # I use a french mac bluetooth keyboard. Pass the following line
    # if you want to swap alt key and command key:
    # `echo options hid_apple swap_opt_cmd=1 | sudo tee -a /etc/modprobe.d/hid_apple.conf`
    # or delete the hid_apple.conf file to remove the swap.
    # Swapping/unswapping require a restart
    # Maybe you should set it with `boot.extraModprobeConfig`
    # See wiki.archlinux.org/index.php?title=Apple_Keyboard&oldid=387451
    xkbModel = "apple_laptop";

    desktopManager.wallpaper.mode = "fill";
    desktopManager.wallpaper.combineScreens = false;
  };
}
