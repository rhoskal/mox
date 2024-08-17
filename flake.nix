{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";

  outputs = { self, nixpkgs }:
    let
      system = "aarch64-darwin";
      pkgs = import nixpkgs { inherit system; };

      haskellDeps = with pkgs.haskellPackages; [
        base
        cabal-install
        hlint
        hspec
      ];

      ghc = pkgs.haskell.compiler.ghc98;
      lsp = pkgs.haskell-language-server.override {
        supportedGhcVersions = [ "98" ];
      };
      nixPackages = [ pkgs.gcc ghc pkgs.ghcid lsp pkgs.ormolu ];
    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = nixPackages ++ haskellDeps;
        shellHook = ''
          mkdir -p .nix-cabal
          export CABAL_DIR=$PWD/.nix-cabal
          export PATH=$PWD/.nix-cabal/bin:$PATH

          alias repl="cabal repl"
        '';
      };
    };
}
