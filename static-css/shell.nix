{ nixpkgs ? import <nixpkgs> {}
, unstable ? import <nixos-unstable> {}
}:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  project = import ./release.nix { pkgs = unstable; };
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    unstable.pkgs.haskellPackages.haskell-language-server
  ];
}
