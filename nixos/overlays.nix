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
}
