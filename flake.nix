{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";

  outputs = { self, nixpkgs, }:
    let
      system = "aarch64-apple-darwin14";
      pkgs = import nixpkgs { inherit system; };
    in {
      packages.${system}.default = pkgs.stdenv.mkDerivation {
        src = ./.;
        pname = "mox";
        isLibrary = true;
        isExecutable = true;
        homepage = "https://github.com/rhoskal/mox";
        license = lib.licenses.bsd3;
        mainProgram = "mox";
      };
    };
}
