{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; config.allowUnfree = true;}
, compiler ? "ghc844"
, haskellPackages ? nixpkgs.haskell.packages.${compiler}
}:
with nixpkgs;
let
  isGHCJS = lib.hasPrefix "ghcjs" compiler;
  atidotHaskellLib = import (gitRoot ./. + /devops/nix/haskell/lib.nix) {};
  ease = package: haskell.lib.doJailbreak (haskell.lib.dontHaddock (haskell.lib.dontCheck package));

  #----
  fixesGHCJS = hspkgs: if isGHCJS then {
    lens             = ease hspkgs.lens;
    comonad          = ease hspkgs.comonad;
    semigroupoids    = ease hspkgs.semigroupoids;
    QuickCheck       = ease hspkgs.QuickCheck;
    tasty-quickcheck = ease hspkgs.tasty-quickcheck;
    scientific       = ease hspkgs.scientific;
    temporary        = ease hspkgs.temporary;
  } else {};

  #----
  atidotAnonymizerSrc = ../anonymizer;

  projectPackages = hspkgs: {
    base-compat-batteries = pkgs.haskell.lib.addBuildDepends hspkgs.base-compat-batteries [ hspkgs.contravariant ];
    insert-ordered-containers = ease (hspkgs.insert-ordered-containers);
    atidot-anonymizer     = hspkgs.callCabal2nix "atidot-anonymizer" "${atidotAnonymizerSrc}" {};
  };
in
haskellPackages.override (old: {
  overrides = pkgs.lib.composeExtensions old.overrides
    (self: hspkgs:
      fixesGHCJS hspkgs
   // projectPackages hspkgs
    );
})
