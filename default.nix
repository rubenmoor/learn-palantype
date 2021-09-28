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
    # clay = pkgs.haskellPackages.callHackage "clay" "0.13.3" {};
in
  with obelisk;
  project ./. ({ ... }: {
    android.applicationId = "systems.obsidian.obelisk.examples.minimal";
    android.displayName = "Obelisk Minimal Example";
    ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    ios.bundleName = "Obelisk Minimal Example";
    staticFiles = import ./static.nix { inherit pkgs; };
    overrides = self: super: {
      reflex-dom = self.callCabal2nix "reflex-dom" (reflex-dom-framework + /reflex-dom) {};
      reflex-dom-core = pkgs.haskell.lib.dontCheck (
        self.callCabal2nix "reflex-dom-core" (
          reflex-dom-framework + /reflex-dom-core
        ) {}
      );
      servant-reflex = self.callCabal2nix "servant-reflex" (pkgs.fetchFromGitHub {
        owner = "imalsogreg";
        repo = "servant-reflex";
        rev = "20e2621cc2eca5fe38f8a01c7a159b0b9be524ea";
        sha256 = "0aqyk04yg39xj40aj86hr6gwbzvj6i2fxi8zznmfl5fay8l96b4g";
      }) {};
      servant-snap = pkgs.haskell.lib.dontCheck (self.callCabal2nix "servant-snap" (pkgs.fetchFromGitHub {
        owner = "haskell-servant";
        repo = "servant-snap";
        rev = "b54c5da86f2f2ed994e9dfbb0694c72301b5a220";
        sha256 = "0j0a3lznxnf8f98fibla7d0bksz3kk4z9q02afmls5f9yylpf2ad";
      }) {});
    };
  })
