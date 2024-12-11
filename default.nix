{
  pkgs ? import (fetchTarball {
    # nixos-unstable rev 2024-12-03
    url = "https://github.com/NixOS/nixpkgs/archive/55d15ad12a74eb7d4646254e13638ad0c4128776.tar.gz";
  }) { },
}:

let
  haskellPackages = pkgs.haskell.packages.ghc948;
  haskellDeps = [
    haskellPackages.base
    haskellPackages.parsec
    haskellPackages.filepath
    haskellPackages.pretty-show
    haskellPackages.prettyprinter
    haskellPackages.pretty-show
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
