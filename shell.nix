{
  pkgs ? import (fetchTarball {
    # nixos-unstable rev 2024-12-03
    url = "https://github.com/NixOS/nixpkgs/archive/55d15ad12a74eb7d4646254e13638ad0c4128776.tar.gz";
  }) { },
}:

let
  haskellator = pkgs.callPackage ./default.nix { };
  corpus = pkgs.callPackage ./rtlil-corpus/default.nix { };
in
pkgs.callPackage (
  { mkShell }:
  mkShell {
    CORPUS = corpus;
    nativeBuildInputs = [
      haskellator
    ];
  }
) { }
