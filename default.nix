{ system ? builtins.currentSystem
, pkgs ? import <nixpkgs> {}
}:
let
  pkgs20_09 = import (pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "20.09";
    sha256 = "1wg61h4gndm3vcprdcg7rc4s1v3jkm5xd7lw8r2f67w502y94gcy";
  }) {};
  pkgs21_05 = import (pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "21.05";
    sha256 = "1ckzhh24mgz6jd1xhfgx0i9mijk6xjqxwsshnvq789xsavrmsc36";
  }) {};
  obelisk = (import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "13.2";
    terms.security.acme.acceptTerms = true;
  });

in
  with pkgs.haskell.lib;
  obelisk.project ./. ({ ... }: {
    android.applicationId = "systems.obsidian.obelisk.examples.minimal";
    android.displayName = "Palantype";
    ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    ios.bundleName = "Palantype";

    staticFiles = import ./static.nix { inherit pkgs; };

    shellToolOverrides = self: super: {
      inherit (pkgs20_09.haskell.packages.ghc865) haskell-language-server;
    };

    overrides = self: super: {

      mkDerivation = args: super.mkDerivation (args // {
          doCheck = false;
          doHaddock = false;
          enableLibraryProfiling = false;
        });
      # for persistent
      lift-type = self.callHackage "lift-type" "0.1.0.1" {};
      # esqueleto's test suite introduces dependencies to persistent-... packages
      esqueleto = self.callHackage "esqueleto" "3.5.3.0" {};
      persistent = self.callHackage "persistent" "2.13.2.1" {};
      # the version 2.13.0.2 still got a bug with mariadb
      #persistent-mysql = self.callHackage "persistent-mysql" "2.13.0.2" {};
      persistent-mysql = self.callHackageDirect {
        pkg = "persistent-mysql";
        ver = "2.13.0.3";
        sha256 = "0mnrgq05nk7ghfavfr2rhcz2yc0lciw88idri0ljsk2nymsnrbb8";
      } {};

      gerippe = self.callCabal2nix "gerippe" (pkgs.fetchFromGitHub {
        owner = "rubenmoor";
        repo = "gerippe";
        rev = "041f32de5094589ebc7dfe2bfd6e8078470ef2b6";
        sha256 = "1mfxxn0q09wa81bdw4ngisklaaib4lz1ibxayrpk7g7l44hy92sm";
      }) {};
      #gerippe = self.callCabal2nix "gerippe" ../../gerippe {};

      # trying to switch to jose 0.8.5.1, but didn't make a difference
      #websockets = doJailbreak super.websockets;
      #jsaddle = doJailbreak super.jsaddle;
      #base64-bytestring = self.callHackage "base64-bytestring" "1.2.1.0" {};
      #jose = self.callHackage "jose" "0.8.5.1" {};

      servant-reflex = self.callCabal2nix "servant-reflex" (pkgs.fetchFromGitHub {
        owner = "imalsogreg";
        repo = "servant-reflex";
        rev = "a6f859e52857e0eda753cf113fabfff61f06da6a";
        sha256 = "0j36sl7l553iy1vpwy6263xdpj3m2n2rkkkdcsxpkr48p328lac4";
      }) {};
      #servant-reflex = self.callCabal2nix "servant-reflex" ../servant-reflex { };
      servant-snap = self.callCabal2nix "servant-snap" (pkgs.fetchFromGitHub {
        owner = "haskell-servant";
        repo = "servant-snap";
        rev = "b54c5da86f2f2ed994e9dfbb0694c72301b5a220";
        sha256 = "0j0a3lznxnf8f98fibla7d0bksz3kk4z9q02afmls5f9yylpf2ad";
      }) {};
      #my-palantype = self.callCabal2nix "my-palantype" ../my-palantype { };
      my-palantype = dontCheck (self.callCabal2nix "my-palantype" (pkgs.fetchFromGitHub {
        owner = "rubenmoor";
        repo = "my-palantype";
        rev = "aa72cdf5c04aa8818863254524870c2225ed6f62";
        sha256 = "1scp62rvdwpa20lxfdbdazkbjfdbpfijy37l4v807lvd1l4z8k6f";
      }) {});
    };
  })
