{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";

  outputs = { self, nixpkgs }:
    let
      system = "aarch64-darwin";
      pkgs = import nixpkgs { inherit system; };

      haskellDeps = with pkgs.haskellPackages; [ base cabal-install hlint hspec ];

      ghc = pkgs.haskell.compiler.ghc98;
      nixPackages =
        [ pkgs.gcc ghc pkgs.ghcid pkgs.haskell-language-server pkgs.ormolu ];
    in {
      packages.${system}.default = pkgs.stdenv.mkDerivation {
        name = "mox";
        src = ./.;
        buildInputs = nixPackages ++ haskellDeps;
        shellHook = ''
          mkdir -p .nix-cabal
          export CABAL_DIR=$PWD/.nix-cabal
          export HIE_BIOS_GHC=$CABAL_DIR/.cache
          export PATH=$PWD/.nix-cabal/bin:$PATH
        '';
      };
    };
}
