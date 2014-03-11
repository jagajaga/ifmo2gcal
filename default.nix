{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callPackage ./ifmo2gcal.nix {}
