with import (import ./nix/sources.nix).nixpkgs {};

(import ./. { hoo = true; }).env.overrideAttrs (old: {
  buildInputs = [
    haskellPackages.haskell-language-server
    cabal-install
    cabal2nix
  ];
})
