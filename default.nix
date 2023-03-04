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
    useGHC810 = true;
  });

in
  with pkgs.haskell.lib;
  obelisk.project ./. ({ ... }: {
    # android.applicationId = "systems.obsidian.obelisk.examples.minimal";
    # android.displayName = "Palantype";
    # ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    # ios.bundleName = "Palantype";

    staticFiles = import ./static.nix { inherit pkgs; };

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

      #reflex-dom-pandoc = self.callHackage "reflex-dom-pandoc" "1.0.0.0" {};
      # waiting for my pull request to make it into upstream
      reflex-dom-pandoc = self.callCabal2nix "reflex-dom-pandoc" (pkgs.fetchFromGitHub {
        owner = "rubenmoor";
        repo = "reflex-dom-pandoc";
        rev = "7878b040d15ba9d327a56e673231c6467d7c5973";
        sha256 = "111x4qs958jq38vrm56m4jl4jrww3jjw2kyd2l2clwmxp7wipkz0";
      }) {};

      #servant-reflex = self.callCabal2nix "servant-reflex" ../servant-reflex {};
      servant-reflex = self.callCabal2nix "servant-reflex" (pkgs.fetchFromGitHub {
        owner = "rubenmoor";
        repo = "servant-reflex";
        rev = "00b101c46fb3ecb3d35fa55080b4b32455cee66e";
        sha256 = "1gghpfgsw8p2v6msz17sr5nn30pkg2c59pz7gvazqrv80v8102bb";
      }) {};

      #servant-snap = self.callCabal2nix "servant-snap" ../servant-snap {};
      servant-snap = self.callCabal2nix "servant-snap" (pkgs.fetchFromGitHub {
        owner = "rubenmoor";
        repo = "servant-snap";
        rev = "657af13efa002b4f7c24bfc20d63d59e85a1086f";
        sha256 = "0p1h1a1rnrg5c63cpir8i26w93af6hqwxgqc6w2h94wjz2fbxp4c";
      }) {};

      my-palantype = self.callCabal2nix "my-palantype" ../my-palantype { };
      #my-palantype = self.callCabal2nix "my-palantype" (pkgs.fetchFromGitHub {
      #  owner = "rubenmoor";
      #  repo = "my-palantype";
      #  rev = "a0a10c93736f30aa74dfb758e84dca3e88a3eb63";
      #  sha256 = "1v9zsrmz1zwn0hanb09kg8w78xk9jns1f2r38wmyzn5v2h9hvqmm";
      #}) {};

      bytestring-trie = self.callHackage "bytestring-trie" "0.2.7" {};
    };
  })
