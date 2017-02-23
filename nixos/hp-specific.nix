# On my hp laptop, symbolic link this file with `specific.nix`.
{ config, lib, pkgs, ... }:

{
  # -------------------------------------------------- Hardware configuration
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/8a50958b-bff3-421a-968e-42903b15a28a";
      fsType = "ext4";
      # Supposedly better for SSD
      options = [ "noatime" "nodiratime" "discard" ];
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/0E92-DE8F";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/f1315023-2d2c-4e08-964c-46731da945ba"; }
    ];

  nix.maxJobs = lib.mkDefault 16;

  # Localization of my encrypted partition
  # https://gist.github.com/martijnvermaat/76f2e24d0239470dd71050358b4d5134#file-nixos-md
  boot.initrd.luks.devices = [{
    name = "root";
    device = "/dev/sda2";
    preLVM = true;
    allowDiscards = true;
  }];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # -------------------------------------------------- System configuration

  # Networking stuff
  networking = {
    hostName = "hp-rfish";
    # TODO: Delete the support of network-manager and only relies on
    # wpa-supplicant.
    # wireless.enable = true;
    networkmanager.enable = true;
  };

  # Enable CUPS to print documents.
  # Find PPD Drivers on https://www.openprinting.org/printers
  services.printing = {
    enable = true;
    webInterface = true; # Access web interface at
                         #  `services.printing.listenAddresses`
                         # localhost:631
  };

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

    displayManager.sessionCommands = ''
      # xrandr --dpi 144 --output eDP1 --auto --output DP1-1 --auto --scale 1.35x1.35 --right-of eDP1
      xrandr --output DP2 --auto --output DP1-1 --auto --right-of DP2

      # pexels.com
      ${pkgs.feh}/bin/feh --bg-fill '/home/rfish/Sync/Pictures/sea-ocean-sailing-ship-boat.jpg'
    '';
  };
}
