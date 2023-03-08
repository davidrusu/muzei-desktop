let
  nixpkgs = import <nixpkgs> {};
in
  with nixpkgs;
  stdenv.mkDerivation {
    name = "haskell";
    buildInputs = [
     feh
     ghc
     curl
     haskellPackages.cabal-install
    ];
  }
