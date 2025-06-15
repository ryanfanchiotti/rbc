{ pkgs ? import <nixpkgs> {} }:

# Create a shell with the necessary packages to run bc
pkgs.mkShell {
  nativeBuildInputs = with pkgs.buildPackages; [ 
    cabal-install
    ghc
  ];
}
