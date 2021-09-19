{ system ? builtins.currentSystem
, pkgs ? import <nixpkgs> {}
}:
let reflex-dom-framework = pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-dom";
      rev = "6a7782a61e90e7369a8278441eb47f702bb7c63b";
      sha256 = "13y2h9cqhll55qgk7x33wnz88822irkdxych1c0fbw20jghhp96h";
    };
    reflex-platform-hls = pkgs.fetchFromGitHub {
      # branch: haskell-language-server
      owner = "ibizaman";
      repo = "reflex-platform";
      rev = "a9cd44d288395092fdaa76a6a7f146049cc21f15";
      sha256 = "1nfzls18cs0a1a0d3xiiidsl0n6clcf5z9i8b78yj1k8hdc6k1fh";
    };
    obelisk = (import ./.obelisk/impl {
      reflex-platform-func = args@{ ... }: (import reflex-platform-hls) (args // {
          inherit system;
          # activate haskell-language-server for reflex-platform with full
          # support for template-haskell
          hlsSupport = true;
      });
      inherit system;
      iosSdkVersion = "13.2";
      terms.security.acme.acceptTerms = true;
    });
in
  with obelisk;
  project ./. ({ ... }: {
    android.applicationId = "systems.obsidian.obelisk.examples.minimal";
    android.displayName = "Obelisk Minimal Example";
    ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    ios.bundleName = "Obelisk Minimal Example";
    overrides = self: super: {

    };
  })
