{ config, options, lib, pkgs, ... }:

{
  services.syncthing = {

    enable = true;

    openDefaultPorts = true;

    user = "rfish";
    configDir = "${config.users.users.rfish.home}/.config/syncthing";

    declarative.devices = {
      home = { id = "6IWZNUJ-S3TO23F-SHT6UGV-MFX3U5O-S5DNPT2-MEEWFVN-ZUPFIPG-YMAKFAI"; };
      hp = { id = "U3CRJ5I-KFRTLQP-SYMPZ6H-PTEK535-FUMPKEG-KLBFJQI-4KPVSCA-3LLDLAP"; };
      jolla = { id = "4LLHNME-57MZXFN-Z5C6WT4-53BQYXE-CRYFLUQ-F65UVPE-GPQSOUZ-HEW3EQF"; };
      donatello = { id = "EPFSPKC-CQAEOMP-F7MNWBV-2N4FGA5-446F4MF-34GJD3B-DXPAPAQ-A6PBEQL"; };
    };

    declarative.folders = {
      "${config.users.users.rfish.home}/secret" = {
        id = "secret";
        devices = [ "home" "hp" "jolla" "donatello" ];
      };
      # "${config.users.users.rfish.home}/Sync" = {
      #   enable = false;
      #   id = "default";
      #   devices = [ "home" "hp" "jolla" ];
      # };
    };
  };
}
