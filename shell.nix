{ nixpkgs ? import <nixpkgs> { }, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, alex, array, base, hspec, lib, QuickCheck, text }:
    mkDerivation {
      pname = "mox";
      version = "0.1.0";
      src = ./.;
      isLibrary = true;
      isExecutable = true;
      libraryHaskellDepends = [ array base text ];
      libraryToolDepends = [ alex ];
      executableHaskellDepends = [ base ];
      testHaskellDepends = [ base hspec QuickCheck ];
      homepage = "https://github.com/rhoskal/mox";
      license = lib.licenses.bsd3;
      mainProgram = "mox";
    };

  haskellPackages = if compiler == "default" then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f { });

in if pkgs.lib.inNixShell then drv.env else drv
