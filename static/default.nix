{ pkgs ? import <nixpkgs> {} }:
let
  frontendSrcFiles = ../frontend/src;
  # tailwind = (import ../../tailwind-haskell).packages.x86_64-linux.tailwind;
  tailwind-src = pkgs.fetchFromGitHub {
    owner = "rubenmoor";
    repo = "tailwind-haskell";
    rev = "b7dbf133da60fb27f14981be225382065477759f";
    sha256 = "0fvwq5x2rk4agz0kdqalkkg8yxvr1haimj1v4sjl8iggb9nm0xrg";
  };
  tailwind-package = import tailwind-src;
  tailwind = tailwind-package.packages.x86_64-linux.tailwind;
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
