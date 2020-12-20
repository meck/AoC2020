{ pkgs ? import <nixpkgs> { }, compiler ? "default" }:
let

  myHaskellPkgs = if compiler == "default" then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

in myHaskellPkgs.callCabal2nix "AoC2020" (builtins.fetchGit ./.) { }
# in myHaskellPkgs.callCabal2nix "AoC2020" (./.) { }

