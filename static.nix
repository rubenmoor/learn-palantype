{ pkgs ? import <nixpkgs> {} }:
let
  staticCss = import ./static-css/release.nix {};
in pkgs.stdenv.mkDerivation {
  name = "static";
  src = ./static-files;
  buildInputs = [ staticCss ];
  installPhase = ''
    mkdir -p $out
    ${staticCss}/bin/static-css > $out/main.css
    if [ "$(ls -A)" ]
      then cp -r * $out/
    fi
  '';
}
