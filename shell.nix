with import <nixpkgs> {};

{
  LispyEnv = stdenv.mkDerivation {
    name = "LispyEnv";
    buildInputs = [
      ghc
      cabal-install
      haskellPackages.ghcid
    ];
  };
}
