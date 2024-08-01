{ mkDerivation, attoparsec, base, directory, filepath, lib
, pretty-show, prettyprinter, tasty, tasty-golden, text
}:
mkDerivation {
  pname = "haskellator";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    attoparsec base filepath pretty-show prettyprinter text
  ];
  executableHaskellDepends = [
    attoparsec base filepath pretty-show prettyprinter text
  ];
  testHaskellDepends = [
    attoparsec base directory filepath pretty-show prettyprinter tasty
    tasty-golden text
  ];
  homepage = "https://github.com/JoyOfHardware/haskellator#readme";
  license = lib.licenses.gpl3Only;
  mainProgram = "rtlil-parse";
}
