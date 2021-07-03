{ nixpkgs ? import ./nixpkgs_pinned.nix
, compiler ? "ghc8104"
}:

let
    pkgs = nixpkgs;
in
    pkgs.haskell.packages.${compiler}.callPackage ./cabal2nix_generated_derivation.nix { }

# To generate ./cabal2nix_generated_derivation.nix
# cd nix && nix-shell --pure -p cabal2nix --run "cabal2nix .." > cabal2nix_generated_derivation.nix && cd ..
