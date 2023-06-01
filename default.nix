{ system ? builtins.currentSystem
}:
let
  pkgs = import (builtins.fetchGit {
    name = "nixos-unstable-2023-02-26";
    url = "https://github.com/nixos/nixpkgs/";
    ref = "refs/heads/nixos-unstable";
    rev = "7f5639fa3b68054ca0b062866dc62b22c3f11505";
  }) {};
  obelisk = (import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "13.2";
    terms.security.acme.acceptTerms = true;
    # useGHC810 = true;
  });
  # persistent 2.14.0.5
  persistent-src = pkgs.fetchFromGitHub {
    owner = "yesodweb";
    repo = "persistent";
    rev = "434f49266749cad6f074920fb61ad3ec6ef90bea";
    sha256 = "1h1hflfddr0k1ir4q2yf2a6wh9ix3qjqpbx3356117n59h6jf8f6";
  };

in
  with pkgs.haskell.lib;
  obelisk.project ./. ({ ... }: {
    # android.applicationId = "systems.obsidian.obelisk.examples.minimal";
    # android.displayName = "Palantype";
    # ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    # ios.bundleName = "Palantype";

    staticFiles = import ./static { inherit pkgs; };

    shellToolOverrides = self: super: {
      inherit (pkgs.haskellPackages) cabal-plan cabal-install;
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
      esqueleto = self.callHackage "esqueleto" "3.5.8.0" {};

      #persistent = self.callHackage "persistent" "2.13.3.5" {};
      persistent = self.callCabal2nix "persistent" "${persistent-src}/persistent" {};
      persistent-mysql = self.callCabal2nix "persistent-mysql" "${persistent-src}/persistent-mysql" {};
      mysql = self.callHackage "mysql" "0.2.1" {};
      mysql-simple = self.callHackage "mysql-simple" "0.4.9" {};
      # the version 2.13.0.2 still got a bug with mariadb
      #persistent-mysql = self.callHackage "persistent-mysql" "2.13.0.2" {};
      #persistent-mysql = self.callHackageDirect {
      #  pkg = "persistent-mysql";
      #  ver = "2.13.0.3";
      #  sha256 = "0mnrgq05nk7ghfavfr2rhcz2yc0lciw88idri0ljsk2nymsnrbb8";
      #} {};

      gerippe = self.callCabal2nix "gerippe" (pkgs.fetchFromGitHub {
        owner = "rubenmoor";
        repo = "gerippe";
        rev = "a25ccf56afe17ae5870955dbe5b968325dea8e24";
        sha256 = "1ny1kacfd4vx0sbkxzlrz6ixkwxlinmfm5k3x184a0m5si5qb7n7";
      }) {};
      #gerippe = self.callCabal2nix "gerippe" ../../gerippe {};

      gridtables = self.callHackageDirect {
        pkg = "gridtables";
        ver = "0.1.0.0";
        sha256 = "zMQW1jbntZ1pxWKT+D+tt/LKy7SkADFN3o+Wn3VdD88=";
      } {};

      jira-wiki-markup = self.callHackageDirect {
        pkg = "jira-wiki-markup";
        ver = "1.5.0";
        sha256 = "a0/b2AfcA7mMmfrcbEEh5pNUq/OjJw4H7uLTpkl+Y1c=";
      } {};
      #pandoc-types = self.callCabal2nix "pandoc-types" ../pandoc-types {};
      pandoc-types = self.callHackageDirect {
        pkg = "pandoc-types";
        ver = "1.23";
        sha256 = "BfwTC6irH6/dYIuqd+rMqLNzH8DP1g/7mFJhZZ9O9Fo=";
      } {};

      commonmark-pandoc = self.callHackageDirect {
        pkg = "commonmark-pandoc";
        ver = "0.2.1.3";
        sha256 = "0JXjvqZVFa8RR64Wi9NZ97tQ9S900qRaE6/4adQaQaU=";
      } {};
      commonmark = self.callHackage "commonmark" "0.2.2" {};
      commonmark-extensions = self.callHackageDirect {
        pkg = "commonmark-extensions";
        ver = "0.2.3.3";
        sha256 = "J0sdUT4X0yjJ4iAu/ne7EA4IzFAoyBWf59hx5FyOsH4=";
      } {};
      unicode-data = self.callHackageDirect {
        pkg = "unicode-data";
        ver = "0.3.1";
        sha256 = "p3uoLleamcIYEqe9c+EFRO3mPMZGgLf9XQ3/qwdmiF8=";
      } {};

      texmath = self.callHackageDirect {
        pkg = "texmath";
        ver = "0.12.6";
        sha256 = "r5R3TfWOYfgMH4JDXMX14JPykGjKXuJn6IovJ4rSnTo=";
      } {};
      citeproc = self.callHackageDirect {
        pkg = "citeproc";
        ver = "0.8.1";
        sha256 = "XhaAQo0kYCZzenyUftgiCqNs/5esca2udcfeSpBOTjI=";
      } {};

      doclayout = self.callHackage "doclayout" "0.4" {};
      emojis = self.callHackage "emojis" "0.1.2" {};
      doctemplates = self.callHackageDirect {
        pkg = "doctemplates";
        ver = "0.11";
        sha256 = "8xT3CJzWjhLEHTBcbl+BCF0lc5fVQUQ0IRdLMNmthmc=";
      } {};
      ipynb = self.callHackage "ipynb" "0.2" {};
      mime-types = self.callHackageDirect {
        pkg = "mime-types";
        ver = "0.1.1.0";
        sha256 = "HnBsPBdONbaw42/JnpONXAIwsEwc/fMyUXcm4cJdWUQ=";
      } {};

      pandoc = self.callCabal2nix "pandoc" (pkgs.fetchFromGitHub {
        owner = "rubenmoor";
        repo = "pandoc";
        rev = "8fa46a2e5701db1ed579cc65378a927746ca8586";
        sha256 = "0vl1ikgg80rkwdz6chj2s2h5xdkylvi4ymcziq3v0s62pnqf8bk3";
      }) {};

      skylighting-core = self.callHackage "skylighting-core" "0.9" {};
      skylighting = self.callHackage "skylighting" "0.9" {};

      #reflex-dom-pandoc = self.callCabal2nix "reflex-dom-pandoc" ../reflex-dom-pandoc {};
      # waiting for my pull request to make it into upstream
      reflex-dom-pandoc = self.callCabal2nix "reflex-dom-pandoc" (pkgs.fetchFromGitHub {
        owner = "rubenmoor";
        repo = "reflex-dom-pandoc";
        rev = "050388e68dbae52043e0d633f854eb98257fc223";
        sha256 = "sha256-6UViAujrl7odTPvnseIx+x+/9HBhoKFbeM8Yo2P3iwQ=";
      }) {};

      #servant-reflex = self.callCabal2nix "servant-reflex" ../servant-reflex {};
      servant-reflex = self.callCabal2nix "servant-reflex" (pkgs.fetchFromGitHub {
        owner = "rubenmoor";
        repo = "servant-reflex";
        rev = "06bd09d0777c658da5269840be60d6cb24a264e8";
        sha256 = "0s23q4cz93dw8wwz3n94l69qw5syvh37hj7bgdy5i0bb8b9g2617";
      }) {};

      #servant-snap = self.callCabal2nix "servant-snap" ../servant-snap {};
      servant-snap = self.callCabal2nix "servant-snap" (pkgs.fetchFromGitHub {
        owner = "rubenmoor";
        repo = "servant-snap";
        rev = "4850305bd586e887229954ad9dfccb649c497f8e";
        sha256 = "1ins0j4vd3cmf2dgh601b4wjqky79myz1245v2b59m5zsp11am01";
      }) {};

      #my-palantype = self.callCabal2nix "my-palantype" ../my-palantype { };
      my-palantype = self.callCabal2nix "my-palantype" (pkgs.fetchFromGitHub {
        owner = "rubenmoor";
        repo = "my-palantype";
        rev = "2afe58feed95771c2094fb797589286b1b84b522";
        sha256 = "077cgzf7nd458sdy9aarp2nv1x5mfrb7wmzchqrz5yh4gp8s6l42";
      }) {};

      bytestring-trie = self.callHackage "bytestring-trie" "0.2.7" {};

      # dunno why, if not explicitly specified trying and failing with 1.1.1
      lens-aeson = self.callHackage "lens-aeson" "1.2.2" {};
      jose = self.callHackage "jose" "0.9" {};
      lens = self.callHackage "lens" "5.1.1" {};
      servant = self.callHackage "servant" "0.19.1" {};
      base64-bytestring = self.callHackage "base64-bytestring" "1.2.1.0" {};
      servant-auth = self.callHackage "servant-auth" "0.4.1.0" {};
      # jsaddle-dom doesn't have a version that supports lens 5.*
      jsaddle-dom = doJailbreak(self.callHackage "jsaddle-dom" "0.9.5.0" {});
      # reflex-dom-core doesn't support lens > 5.1
      reflex-dom-core = doJailbreak(super.reflex-dom-core);
    };
  })
