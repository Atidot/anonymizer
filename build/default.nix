{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; config.allowUnfree = true; }
, compiler ? "ghc844"
}:
with nixpkgs;
let
  haskellPackages = import ./haskell.nix { inherit nixpkgs compiler; };

  haskellEnv = haskellPackages.ghcWithPackages (ps: with ps; [
    atidot-anonymizer
  ]);

in
stdenv.mkDerivation rec {
  name = "atidot-anonymizer";

  env = buildEnv { name = name; paths = buildInputs; };
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup; ln -s $env $out
  '';

  buildInputs = [
    haskellEnv
  ];
}
