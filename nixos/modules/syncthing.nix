{ config, options, lib, pkgs, ... }:

with lib;

{
  services.syncthing =
    let
      device-name = config.networking.hostName;
    in {
      enable = true;
      user = "rfish";
      configDir = "${config.users.users.rfish.home}/.config/syncthing";
      openDefaultPorts = true;

      # I have to copy this from my private secret folder
      cert = builtins.toPath "/etc/nixos/secret/${device-name}-syncthing-cert.pem";
      key = builtins.toPath "/etc/nixos/secret/${device-name}-syncthing-key.pem";

      devices = {
        home =      { id = "6IWZNUJ-S3TO23F-SHT6UGV-MFX3U5O-S5DNPT2-MEEWFVN-ZUPFIPG-YMAKFAI"; };
        hp =        { id = "U3CRJ5I-KFRTLQP-SYMPZ6H-PTEK535-FUMPKEG-KLBFJQI-4KPVSCA-3LLDLAP"; };
        jolla =     { id = "RH4MLPV-RUN3EUH-GDL6YVB-2GAD0VZ-JVYZ4SA-WP25PTB-XHCVKW4-VINKRQR"; };
        donatello = { id = "EPFSPKC-CQAEOMP-F7MNWBV-2N4FGA5-446F4MF-34GJD3B-DXPAPAQ-A6PBEQL"; };
      };

      folders = {
        "${config.users.users.rfish.home}/secret" = {
          id = "secret";
          devices = [ "home" "hp" "jolla" "donatello" ];
          ignorePerms = false;
        };
        "${config.users.users.rfish.home}/Sync" = {
          id = "default";
          devices = [ "home" "hp" "donatello" ];
        };
      };
    };
}
