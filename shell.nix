{ pkgs ? import (fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/24.05.tar.gz";
}) {} }:

let
  haskellator = pkgs.callPackage ./default.nix {};
in
pkgs.mkShell {
  buildInputs = [
    haskellator
    ];
}
