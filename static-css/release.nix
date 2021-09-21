{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callCabal2nix "static-css" ./. {}
