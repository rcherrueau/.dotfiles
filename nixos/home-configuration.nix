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
  boot.loader.timeout = 1;
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

  # Note: Remember to change firewall configuration if I start a local 
  # server to get files
  # > nix shell nixpkgs#python3Minimal -c  python -m http.server 8081
  # networking.firewall.allowedTCPPorts = [ 8081 ];

  environment.systemPackages = with pkgs; [
    ntfs3g
  ];

  hardware.bluetooth.enable = true;

  hardware.opengl.extraPackages = with pkgs; [ vaapiIntel vaapiVdpau ];

  services.xserver = {
    videoDrivers = [ "nvidia" ];

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
