{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = ./addresses.nix;
  
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

  devTools = [
    haskellPackages.ghcid
  ];

  shellDrv =
    pkgs.haskell.lib.overrideCabal
      drv
      (drv': {
        buildDepends = (drv'.buildDepends or []) ++
          [ (haskellPackages.hoogleLocal {
              packages =
                (drv'.libraryHaskellDepends or []) ++
                (drv'.executableHaskellDepends or [])++
                (drv'.testHaskellDepends or []);
              })
          ] ++
          devTools;
});

in

  if pkgs.lib.inNixShell then shellDrv.env else drv
