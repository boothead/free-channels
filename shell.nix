
let
  pkgs = import <nixpkgs> {};
  hs = pkgs.haskellPackages;
  stdenv = pkgs.stdenv;
in rec {
  freeEnv = stdenv.mkDerivation rec {
    name = "free-env";
    version = "0.0.0.1";
    src = ./.;
    buildInputs = [ hs.ghc hs.free ];
  };
}
