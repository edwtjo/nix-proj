{ nixpkgs ? import <nixpkgs> {}, compiler ? "lts-5" }:
let
  drv = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./. { };
in
drv
