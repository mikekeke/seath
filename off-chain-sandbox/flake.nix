{
  description = "off-chain-communication-prototype";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-22.05";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs@{ self, nixpkgs, utils, ... }:
    (utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        shell = pkgs.mkShell {
          buildInputs = with pkgs;
            [
              ghc
              cabal-install
              haskell-language-server
              nixpkgs-fmt
              zlib
              haskellPackages.cabal-fmt
              haskellPackages.fourmolu
              haskellPackages.implicit-hie
              haskellPackages.pointfree
            ];
          LC_CTYPE = C.UTF-8;
          LC_ALL = C.UTF-8;
          LANG = C.UTF-8;
        };
      in
      {
        devShells.default = shell;
      })
    );
}
