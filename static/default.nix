{ pkgs ? import <nixpkgs> {} }:
let
  frontendSrcFiles = ../frontend/src;
  tailwind = (import ../../tailwind-haskell).packages.x86_64-linux.tailwind;
in pkgs.stdenv.mkDerivation {
  name = "static";
  src = ./.;
  buildInputs = [ tailwind ];
  installPhase = ''
    mkdir -p $out

    tailwind-run '${frontendSrcFiles}/**/*.hs' \
                 --output $out/styles.css \
                 --theme theme.js \
                 --css input.css \
                 --custom-plugins plugins.js

    cd ./files
    if [ "$(ls -A)" ]
      then cp -r * $out/
    fi
  '';
}
