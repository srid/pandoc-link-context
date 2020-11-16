{ pkgs ? import ./dep/nixpkgs {} }:

let 
  inherit (import ./dep/gitignore { inherit (pkgs) lib; }) gitignoreSource;
in 
  pkgs.haskellPackages.developPackage {
    name = "pandoc-link-context";
    root = gitignoreSource ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          cabal-fmt
          ghcid
          haskell-language-server
        ]);
  }