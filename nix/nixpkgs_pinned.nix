# JSON file generated using
# nix-prefetch-git https://github.com/nixos/nixpkgs.git refs/heads/nixpkgs-unstable > ./nixpkgs_pinned.json

let
  hostNix = import <nixpkgs> {};
  nixpkgsPin = hostNix.pkgs.lib.importJSON ./nixpkgs_pinned.json;
  pinnedPkgs = hostNix.pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgsPin) rev sha256;
  };
in
  import pinnedPkgs {}
