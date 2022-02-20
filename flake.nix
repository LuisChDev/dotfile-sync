{
  description = "sync dotfiles with your device";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=46821ea01c8f54d2a20f5a503809abfc605269d7";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
        jailBreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkgs: pkg.overrideAttrs (_: { meta = { }; }));
        packageName = "dotfile-sync";

      in
      {
        packages.${packageName} = haskellPackages.callCabal2nix packageName self { };
        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
          shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
        };
      }
    );
}
