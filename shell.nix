{ nixpkgs ? import ./nix/nixpkgs_pinned.nix
}:

let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;
  derivation_project = import ./nix/release.nix {inherit nixpkgs;};
in
pkgs.stdenv.mkDerivation {
  name = "json-parser-haskell-env";
  buildInputs = derivation_project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.cabal2nix
    pkgs.which
    pkgs.ghcid
  ];
  shellHook = ''
  echo "generating local cabal config without hackage remote repository"
  echo "forcing user to add haskell packages via nix instead of via cabal downloads"
  export CABAL_DIR="$PWD/_cabal_work"
  export CABAL_CONFIG="$CABAL_DIR/config"
  cabal user-config init --force
  sed --in-place '/hackage/d' $CABAL_CONFIG
  '';
}
