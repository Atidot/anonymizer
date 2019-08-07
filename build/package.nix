{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; config.allowUnfree = true; }
, compiler ? "ghc844"
}:
with nixpkgs;
let
    anonymizer = import ./default.nix {nixpkgs = nixpkgs; compiler = compiler;};
in
   pkgs.haskell.lib.sdistTarball anonymizer