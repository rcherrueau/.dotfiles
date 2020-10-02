{ pkgs, ... }:

# I use a french mac bluetooth keyboard.
#
# To pair the keyboard, put `hardware.bluetooth.enable` to true
# and use `bluetoothctl`:
#   > scan on      # Find MAC of Clavier de apple
#   > pair <MAC>   # Wait for pin Code and enter it, then authorize service
#   > trust <MAC>  # Trust the device to never authorize it again
# See wiki.archlinux.org/index.php?title=Bluetooth&oldid=390632
# See nixos.org/w/index.php?title=FAQ&oldid=24648#How_to_add_a_custom_udev_rule_in_nixos.3F

{
  services.xserver = {
    xkbModel = "apple_laptop";
  };

  boot.kernelModules = [ "hid-apple" ];

  # Keyboard options
  # See https://wiki.archlinux.org/index.php?title=Apple_Keyboard&oldid=628130#Switching_Cmd_and_Alt/AltGr
  # and https://wiki.archlinux.org/index.php?title=Apple_Keyboard&oldid=628130#%3C_and_%3E_have_changed_place_with_^_and_%C2%B0_(or_@_and_#,_or_%60_and_~)
  environment.etc."modprobe.d/hid_apple.conf".text = ''
    # Swap alt and cmd keys
    # echo "1" > /sys/module/hid_apple/parameters/swap_opt_cmd
    options hid_apple swap_opt_cmd=1
    # Get `~` and =`= at `@` and `#`
    # echo "0" > /sys/module/hid_apple/parameters/iso_layout
    options hid_apple iso_layout=0
  '';

  # Reload hid_apple to take into account
  # /etc/modprobre.d/hid_apple.conf options (the hid_apple driver is
  # loaded as part of initrd and therefore the module options are not
  # yet available).
  #
  # Manually:
  # > sudo modprobe -r hid_apple && sudo modprobe hid_apple
  #
  # See https://github.com/NixOS/nixpkgs/issues/20906
  systemd.services.hid-apple = {
    enable = true;
    description = "Take into account hid-apple options";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = [
        "${pkgs.kmod}/bin/modprobe -r hid-apple"
        "${pkgs.kmod}/bin/modprobe hid-apple"
      ];
    };
  };
}
