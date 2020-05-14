# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
#
# Symbolic link this file with `configuration.nix`.
{ config, lib, pkgs, ... }:

{
  # -------------------------------------------------- Hardware configuration
  imports = [ 
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ./desktop-configuration.nix
  ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
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
    device = "/dev/disk/by-uuid/8a50958b-bff3-421a-968e-42903b15a28a";
    fsType = "ext4";
    # Supposedly better for SSD
    options = [ "noatime" "nodiratime" "discard" ];
  };

  fileSystems."/boot" = { 
    device = "/dev/disk/by-uuid/0E92-DE8F";
    fsType = "vfat";
  };

  swapDevices = [ { device = "/dev/disk/by-uuid/f1315023-2d2c-4e08-964c-46731da945ba"; } ];

  nix.maxJobs = lib.mkDefault 16;

  # Localization of my encrypted partition
  # https://gist.github.com/martijnvermaat/76f2e24d0239470dd71050358b4d5134#file-nixos-md
  #
  # See https://github.com/NixOS/nixpkgs/pull/63103
  # boot.initrd.luks.devices = [{
  #   name = "root";
  #   device = "/dev/disk/by-uuid/5da61310-74d9-4ff5-b8e6-7eb9150b20f0";
  #   preLVM = true;
  #   allowDiscards = true;
  # }];
  boot.initrd.luks.devices.root = {
    device = "/dev/disk/by-uuid/5da61310-74d9-4ff5-b8e6-7eb9150b20f0";
    preLVM = true;
    allowDiscards = true;
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # -------------------------------------------------- System configuration

  # Specific packages for my laptop
  environment.systemPackages = with pkgs; [
    rfkill  # Manage wifi device
    feh     # For desktop wallpaper
  ];

  # Networking stuff
  networking = {
    hostName = "hp-rfish";
    # wpa-supplicant.
    wireless.enable = true;
  };

  # Customize sudo:
  # see, nixos-option security.sudo.extraRules
  # https://github.com/NixOS/nixpkgs/blob/b94e1f1fbfb5fe00503b7a9b0e5b1f56b9388b08/nixos/modules/security/sudo.nix#L76
  #
  # The `mkAfter` ensures that this extra rule appears after rules
  # already defined in configuration.nix (especially the %wheel rule
  # that states that all users from the group wheel have to provide a
  # password with sudo).
  security.sudo.extraRules = lib.mkAfter [
    # Let rfish do `sudo rfkill ...` without being prompted for a
    # password.
    { users = [ "rfish" ];
      commands = [ { command = "${pkgs.rfkill}/bin/rfkill";
                     options = [ "NOPASSWD" ]; } ]; }
  ];

  # OpenVPN client:
  services.openvpn.servers = {
    # Access to home freebox. Activate it with `sudo systemctl start
    # openvpn-freebox`
    freebox = {
      autoStart = false;
      config = '' config /home/rfish/openvpn/freeboxVPN.ovpn '';
    };
  };

  # Enable CUPS to print documents.
  # Find PPD Drivers on https://www.openprinting.org/printers
  services.printing = {
    enable = true;
    webInterface = true; # Access web interface at
                         #  `services.printing.listenAddresses`
                         # localhost:631
  };

  # Advanced power management for Linux.
  # See, https://github.com/NixOS/nixos-hardware/tree/b7185cd232c7b9d9e8872ecd4a10e86bac65c0ea/common/pc/laptop
  services.tlp.enable = true;
  powerManagement.cpuFreqGovernor =
    lib.mkIf config.services.tlp.enable (lib.mkForce null);

  # Xorg settings
  services.xserver = {
    videoDrivers = [ "intel" ];

    # monitorSection = ''
    #   DisplaySize 310 174
    # '';

    # Touchpad specific
    # https://github.com/NixOS/nixos/blob/master/modules/services/x11/hardware/synaptics.nix
    synaptics = {
      enable = true;
      minSpeed = "1.0";
      maxSpeed = "3.0";
      twoFingerScroll = true;
      tapButtons = false;
    };

    # Wallpaper: Following are options for the commande
    #
    # > ${pkgs.feh}/bin/feh # --bg-${destkopManager.wallpaper} \
    # >     ${desktopManager.combineScreens ? "" : "--no-xinerama"} \
    # >     $HOME/.background-image
    #
    # https://github.com/NixOS/nixpkgs/blob/766b78841f2342e325e0d801c01ab9d652150a67/nixos/modules/services/x11/desktop-managers/default.nix#L76
    #
    # Find images on pexels.com
    desktopManager.wallpaper.mode = "fill";
    desktopManager.wallpaper.combineScreens = false;

    # LUKS secures My HP boot, so I can safely enable `autoLogin`.
    displayManager.lightdm.autoLogin.enable = true;
  };

  # Use pusleaudio to easily switch between hdmi/display port/analog audio output
  # hardware.pulseaudio.enable = true;

  # Enable virtualization
  virtualisation.libvirtd = {
    enable = true;
    onBoot = "ignore"; # Do not automatically start vms on boot
  };
  users.extraGroups.libvirtd.members = [ "rfish" ];

  # virtualisation.virtualbox.host = {
  #   enable = true;
  #   # Remove GUI and Qt dependency. I use VirtualBox through vagrant.
  #   headless = true;
  #   # `enableExtensionPack` requires to compile VirtualBox extension
  #   # pack, but it takes way too long.
  #   # enableExtensionPack = true;
  # };
  # users.extraGroups.vboxusers.members = [ "rfish" ];
}
