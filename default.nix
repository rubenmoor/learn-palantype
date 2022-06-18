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

      # for persistent
      lift-type = self.callHackage "lift-type" "0.1.0.1" {};
      # esqueleto's test suite introduces dependencies to persistent-... packages
      esqueleto = dontHaddock (dontCheck (self.callHackage "esqueleto" "3.5.3.0" {}));
      persistent = dontCheck (self.callHackage "persistent" "2.13.2.1" {});
      # this version still got a bug with mariadb
      persistent-mysql = dontCheck (self.callHackageDirect {
        pkg = "persistent-mysql";
        ver = "2.13.0.3";
        sha256 = "0jcd3bfm6kcy47iy0z1zbbl8asmy4kvbv1n01g52g550ksgssq5x";
      } {});

      password-instances = dontCheck super.password-instances;

      #gerippe = self.callCabal2nix "gerippe" (pkgs.fetchFromGitHub {
      #  owner = "rubenmoor";
      #  repo = "gerippe";
      #  rev = "da62c0a8e39dca55294bb6ce28c19dece26aec3a";
      #  sha256 = "0gziv9ic9ahwms80q7rg38q92smsckznjjgsnpk2q014gzi7b2j0";
      #}) {};
      gerippe = dontCheck (dontHaddock (self.callCabal2nix "gerippe" ../../gerippe {}));

      servant-reflex = self.callCabal2nix "servant-reflex" (pkgs.fetchFromGitHub {
        owner = "imalsogreg";
        repo = "servant-reflex";
        rev = "a6f859e52857e0eda753cf113fabfff61f06da6a";
        sha256 = "0j36sl7l553iy1vpwy6263xdpj3m2n2rkkkdcsxpkr48p328lac4";
      }) {};
      #servant-reflex = self.callCabal2nix "servant-reflex" ../servant-reflex { };
      servant-snap = dontCheck (self.callCabal2nix "servant-snap" (pkgs.fetchFromGitHub {
        owner = "haskell-servant";
        repo = "servant-snap";
        rev = "b54c5da86f2f2ed994e9dfbb0694c72301b5a220";
        sha256 = "0j0a3lznxnf8f98fibla7d0bksz3kk4z9q02afmls5f9yylpf2ad";
      }) {});
      my-palantype = dontCheck (self.callCabal2nix "my-palantype" ../my-palantype { });
      #my-palantype = dontCheck (self.callCabal2nix "my-palantype" (pkgs.fetchFromGitHub {
      #  owner = "rubenmoor";
      #  repo = "my-palantype";
      #  rev = "971166ceac7753b9e2aa7dbb8d8ec14b9cc8ce11";
      #  sha256 = "1r9v5a0wrp5nq4yw4br989na844d87j47ka7hg5hs1pj01x2amp8";
      #}) {});
    };
  })
