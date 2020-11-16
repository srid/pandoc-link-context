{ pkgs ? import ./dep/nixpkgs {} }:
pkgs.haskellPackages.developPackage {
root = ./.;
modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
    [ cabal-install
        ghcid
        haskell-language-server
    ]);
}