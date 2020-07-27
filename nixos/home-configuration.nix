# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
#
# Symbolic link this file with `configuration.nix`.
{ config, lib, pkgs, ... }:

{
  imports = [
    /etc/nixos/hardware-configuration.nix
    ./desktop-configuration.nix
  ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  # Define your hostname.
  networking.hostName = "home";

  # Connect to my pi-hole
  #
  # A merge is not possible between `unbound.enable = true` from
  # configuration.nix and the following `unbound.enable = false`. The
  # `mkForce` ensures that the disabling of `unbound` takes precedence
  # over the definition in configuration.nix.
  # https://nixos.org/nixos/manual/#sec-modularity
  # services.unbound.enable = lib.mkForce false;
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
    # See http://wiki.archlinux.org/index.php?title=Apple_Keyboard&oldid=387451
    xkbModel = "apple_laptop";

    desktopManager.wallpaper.mode = "fill";
    desktopManager.wallpaper.combineScreens = false;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?
}
