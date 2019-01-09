# Cachix: Binary Cache as a Service 
# - Build Nix packages once and share them for good
#
# https://cachix.org/
#
# The following is getted from `cachix use hie-nix`

{ config, ... }:

{
  nix = {
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://hie-nix.cachix.org"
    ];
    binaryCachePublicKeys = [
      "hie-nix.cachix.org-1:EjBSHzF6VmDnzqlldGXbi0RM3HdjfTU3yDRi9Pd0jTY="
    ];
  };
}

