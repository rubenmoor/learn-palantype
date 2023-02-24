{ system ? builtins.currentSystem
, pkgs ? import <nixos-unstable> {}
}:
let
  obelisk = (import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "13.2";
    terms.security.acme.acceptTerms = true;
    useGHC810 = true;
  });

  pandoc-src = pkgs.fetchFromGitHub {
    repo = "pandoc";
    owner = "jgm";
    rev = "9c39d1632ea9dfd0ff12f15a555baf5b15143274";
    sha256 = "12yxa58izqnbpdrflgklbais8dd5j98vb49zrgxvaljrk567c3sc";
  };

  commonmark-src = pkgs.fetchFromGitHub {
    repo = "commonmark-hs";
    owner = "jgm";
    rev = "869bbce6b5e7e82bfce2ce36a38391176fb0fe4a";
    sha256 = "0ycxbf5m9kxx0ikg53hqzk5hq79vd5xm8pm6li2k3jgx9pl75aks";
  };

in
  with pkgs.haskell.lib;
  obelisk.project ./. ({ ... }: {
    android.applicationId = "systems.obsidian.obelisk.examples.minimal";
    android.displayName = "Palantype";
    ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    ios.bundleName = "Palantype";

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
        rev = "041f32de5094589ebc7dfe2bfd6e8078470ef2b6";
        sha256 = "1mfxxn0q09wa81bdw4ngisklaaib4lz1ibxayrpk7g7l44hy92sm";
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

      pandoc = self.callCabal2nix "pandoc" ../pandoc {};
      # pandoc = doJailbreak(self.callHackageDirect  {
      #   pkg = "pandoc";
      #   ver = "3.1";
      #   sha256 = "R7ZPJ6l/Vh0ieExJBJMqM/pvFDJeUznLIdE8xZSUtus=";
      # } { });

      skylighting-core = self.callHackage "skylighting-core" "0.9" {};
      skylighting = self.callHackage "skylighting" "0.9" {};

      #reflex-dom-pandoc = self.callHackage "reflex-dom-pandoc" "1.0.0.0" {};
      #
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

      # ghc 8.10.7 compatible, not published on hackage
      # TODO: jailbreak shouldn't be necessary
      # check versions of aeson >= 0.7 && <1.5 and base64-bytestring ==1.0.*
      # aeson is actually 1.5.6.0
      # base64-bytestring is 1.1.0.0
      servant-snap = doJailbreak(self.callCabal2nix "servant-snap" (pkgs.fetchFromGitHub {
        owner = "haskell-servant";
        repo = "servant-snap";
        rev = "a30d5d8cbdfed524e93fda6c3dad002e46a49874";
        sha256 = "0s5jizd3k33bh7v7kg79nsaz67jh9hsg490wxy95k4r12r802f17";
      }) {});

      my-palantype = self.callCabal2nix "my-palantype" ../my-palantype { };
      #my-palantype = self.callCabal2nix "my-palantype" (pkgs.fetchFromGitHub {
      #  owner = "rubenmoor";
      #  repo = "my-palantype";
      #  rev = "c258a5f678011dde1ff24ead2b5574c61d6398cb";
      #  sha256 = "0hqd5vbc8wamm4npvsvg06vghv7vcin6pmx15wd9lysi3q7zf2s4";
      #}) {};

      bytestring-trie = self.callHackage "bytestring-trie" "0.2.7" {};
    };
  })
