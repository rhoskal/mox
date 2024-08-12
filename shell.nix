let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };

  haskellDeps = ps: with ps; [ base hspec ];

  ghc = pkgs.haskell.compiler.ghc98 haskellDeps;

  inputs = [
    pkgs.gcc
    pkgs.ghcid
    pkgs.haskellPackages.hlint
    pkgs.llvm
    pkgs.nixfmt
    pkgs.ormolu
  ];

  hooks = ''
    mkdir -p .nix-cabal
    export CABAL_DIR=$PWD/.nix-cabal
    export PATH=$PWD/.nix-cabal/bin:$PATH
  '';
in pkgs.stdenv.mkDerivation {
  name = "app";
  src = ./.;
  buildInputs = inputs;
  shellHook = hooks;
}
