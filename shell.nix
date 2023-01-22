{ pkgs ? import <nixpkgs> {}}:

let
  artax = pkgs.haskellPackages.callCabal2nix "artax" (gitignore ./.) {};
  gitignore = dir: pkgs.nix-gitignore.gitignoreSource [] dir;
in
  artax.env
