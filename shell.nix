{ pkgs ? import <nixpkgs> { }, ... }:
pkgs.mkShell {
  packages = with pkgs;
    [
      (ghc.withPackages
        (pkgs: with pkgs; [ cabal-install haskell-language-server HUnit ]))
    ];
}
