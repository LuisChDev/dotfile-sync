{ pkgs ? import (import ./nix/sources.nix).nixpkgs { }
, hoo  ? false
}:

let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;

in (pkgs.haskellPackages.override (old: {
  overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
    (self: super: {
      ghcWithPackages = super.ghcWithPackages.override (if hoo then { withHoogle = true; } else { });
    });
})).callCabal2nix "dotfile-sync" src { }
