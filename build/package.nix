{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; config.allowUnfree = true; }
, compiler ? "ghc844"
}:
with nixpkgs;

let
  haskellPackages = import ./haskell.nix { inherit nixpkgs compiler; };
  atidot-anonymizer' = haskell.lib.doHaddock haskellPackages.atidot-anonymizer;
in
   pkgs.haskell.lib.sdistTarball atidot-anonymizer'