# Test the overlay with `nix run nixpkgs.emacs`
# Overlays for package overriding. See,
# - https://nixos.wiki/wiki/Overlays
# - https://blog.flyingcircus.io/2017/11/07/nixos-the-dos-and-donts-of-nixpkgs-overlays/
self: super:

{
  emacs = super.emacs.override {
    # Note: Emacs 27.1 supports resizing and rotating images without
    # ImageMagick.  The new function 'image-transforms-p' can be used
    # to test whether any given frame supports these capabilities.  In
    # org, sets the property `#+ATTR_ORG: :width` to change the size
    # of an image.
    cairo = self.cairo;

    # Support Xwidgets for better lsp-ui.
    withXwidgets = true;
  };

  # # Fix by the `location` attribute in `configuration.nix`
  # redshift = super.redshift.override {
  #   # Get location with manual conf. Don't send geoloc with geoclue2
  #   # > pkgs.redshift.configureFlags should output "--enable-geoclue2=no"
  #   withGeolocation = false;
  # };

  # Override some haskell packages
  haskellPackages = super.haskellPackages.override {
      overrides = hself: hsuper: {
        # FIX in https://github.com/NixOS/nixpkgs/pull/95773
        #
        # # Note: Version 50200.10.0 fails to build right now due to a
        # # new feature in vty unavailable on the nix version. Version
        # # 50200.9.0 builds, but the version bound for mattermost-api
        # # is incorrect so we jailbreak mattermost to remove version
        # # bound.
        # #
        # # https://qiita.com/kimagure/items/c3fb87f7f71b9df99078
        # matterhorn = self.haskell.lib.overrideCabal hsuper.matterhorn {
        #   version = "50200.9.0";
        #   sha256 = "1ky022msmh1ashhw8kwxwj4lcswa6xin2537q4bx8miii07cfvaw";
        #   jailbreak = true;
        #   doCheck = false;
        # };
      };
  };
}
