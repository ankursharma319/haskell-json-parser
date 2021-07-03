## 

```
nix-shell shell.nix
cabal v2-build
cabal v2-test
cabal v2-run json-parser-haskell-test
cabal v2-clean
```

Building a release

```
nix-build nix/release.nix --no-out-link
```

## Project creation setup (only for me)

```
nix-prefetch-git https://github.com/nixos/nixpkgs.git refs/heads/nixpkgs-unstable > ./nix/nixpkgs_pinned.json

nix-shell --pure -p ghc cabal-install --run "cabal init --interactive"

nix-shell --pure -p cabal2nix --run "cabal2nix ." > nix/cabal2nix_generated_derivation.nix
```

## Cabal changes

Also update nix derivation via

```
cd nix && \
cabal2nix .. > ./cabal2nix_generated_derivation.nix && \
cd .. && \
direnv reload
```


