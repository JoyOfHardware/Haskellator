{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    (pkgs.haskellPackages.callPackage ./default.nix {})
  ];

  # Optional: Add other tools or dependencies you might need
  shellHook = ''
    echo "Welcome to the haskellator development environment!"
    echo "You can now run the program using `rtlil-parse`."
  '';
}
