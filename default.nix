{ }:
let

  pkgs = import ./nixpkgs.nix;

in pkgs.callPackage ./derivation.nix { pkgs = pkgs; }
