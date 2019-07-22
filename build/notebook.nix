{ gitRoot ? path: if builtins.elem ".git" (builtins.attrNames (builtins.readDir path)) then path else gitRoot (path + "/..")
, atidot ? import (gitRoot ./. + /devops/nix)
, nixpkgs  ? atidot.common.nixpkgs
, compiler ? "ghc844"
}:
with nixpkgs;
let
  jupyterWith = builtins.fetchGit {
    url = https://github.com/tweag/jupyterWith;
    rev = "1176b9e8d173f2d2789705ad55c7b53a06155e0f";
  };
  nixpkgsPath = jupyterWith + "/nix";
  pkgs = import nixpkgsPath { config.allowUnfree = true; };

  haskellPackages = import ./haskell.nix
                  { nixpkgs = pkgs;
                    haskellPackages = pkgs.haskellPackages;
                  };

  jupyter = import jupyterWith { pkgs=pkgs; };

  ihaskellWithPackages = jupyter.kernels.iHaskellWith {
    #extraIHaskellFlags = "--debug";
    haskellPackages=haskellPackages;
    name = "atidot-anonymizer";
    packages = p: with p; [
      xml-conduit
      xml
      hxt
      atidot-anonymizer
    ];
  };

  jupyterEnvironment = jupyter.jupyterlabWith {
    kernels = [ ihaskellWithPackages ];
  };
in
jupyterEnvironment


