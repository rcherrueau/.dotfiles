# Compile emacs with imagemagick so org will support 
# the property `#+ATTR_ORG: :width` that sets the size of an
# image.
#
# Test the overlay with `nix run nixpkgs.emacs`
self: super:

{
  emacs = super.emacs.override {
    imagemagick = super.imagemagick;
  };
}
