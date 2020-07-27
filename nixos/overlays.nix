# Test the overlay with `nix run nixpkgs.emacs`
# Overlays for package overriding. See,
# - https://nixos.wiki/wiki/Overlays
# - https://blog.flyingcircus.io/2017/11/07/nixos-the-dos-and-donts-of-nixpkgs-overlays/
self: super:

{
  emacs = super.emacs.override {
    # Compile emacs with imagemagick so org will support the property
    # `#+ATTR_ORG: :width` that sets the size of an image. Test it
    # with `(image-type-available-p 'imagemagick)` that should return
    # a non-nill value
    imagemagick = self.imagemagick;
    # Support Xwidgets for better lsp-ui.
    withXwidgets = true;
  };

  # # Fix by the `location` attribute in `configuration.nix`
  # redshift = super.redshift.override {
  #   # Get location with manual conf. Don't send geoloc with geoclue2
  #   # > pkgs.redshift.configureFlags should output "--enable-geoclue2=no"
  #   withGeolocation = false;
  # };

  # # Fix https://github.com/NixOS/nixpkgs/issues/79310
  # firefox = self.wrapFirefox (super.firefox-unwrapped.override {
  #   pulseaudioSupport = false;
  #   alsaSupport = true;
  # });

  # Override some haskell packages
  haskellPackages = super.haskellPackages.override {
      overrides = hself: hsuper: {
        # overrideCabal hsuper.matterhorn {
        #   nativeBuildInputs = hsuper.matterhorn.buildInputs ++
        #     [ self.haskell.lib.doJailbreak hsuper.Unique ];
        # };

        # matterhorn = hsuper.matterhorn.override (old: {
        #   buildInputs = builtins.trace ''jq: ${old}, jqr: ${old.jqr or "unavailable"}'' old.buildInputs;
        #   # old.buildInputs ++
        #   #   [ self.haskell.lib.doJailbreak hsuper.Unique ];
        # });

        # Jailbreak Unique for matterhorn (remove version bound).
        #
        # Note: this jailbreaks Unique system-wide (for all haskell
        # packages that use Unique). I guess I could do the same
        # for matterhorn only.
        #
        # https://github.com/NixOS/nixpkgs/pull/92618
        Unique = self.haskell.lib.doJailbreak hsuper.Unique;
      };
  };

  # matterhorn = self.haskell.lib.overrideCabal super.matterhorn (old:
  #   {
  #     buildInputs = old.matterhorn.buildInputs ++
  #       [ self.haskell.lib.doJailbreak super.haskellPackages.Unique ];
  #   });
}
