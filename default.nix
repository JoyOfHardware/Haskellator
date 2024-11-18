{ pkgs ? import (fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/24.05.tar.gz";
}) {} }:

let
  haskellPackages = pkgs.haskell.packages.ghc948;
  haskellDeps = [
    haskellPackages.base
    haskellPackages.parsec
    haskellPackages.filepath
    haskellPackages.pretty-show
    haskellPackages.prettyprinter
  ];
in
haskellPackages.mkDerivation {
  pname = "haskellator";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = haskellDeps;
  executableHaskellDepends = [haskellPackages.base];
  homepage = "https://github.com/JoyOfHardware/haskellator#readme";
  license = pkgs.lib.licenses.gpl3Only;
  mainProgram = "rtlil-parse";
}
