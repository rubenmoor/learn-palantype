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

      gerippe = dontHaddock (self.callCabal2nix "gerippe" (pkgs.fetchFromGitHub {
        owner = "rubenmoor";
        repo = "gerippe";
        rev = "5dedac304b5378eea912f7ce1055793ee108d713";
        sha256 = "0jcd3bfm6kcy47iy0z1zbbl8asmy4kvbv1n01g52g550ksgssq5x";
      }) {});

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
      #  rev = "32c7251529bd8c99369c7bdeabfb943533150e98";
      #  sha256 = "0iy66zic7qayy2r5jkcg5sppylc4jq5za15bd5h8sgina7lww7am";
      #}) {});
    };
  })
