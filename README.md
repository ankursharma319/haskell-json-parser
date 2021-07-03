# Simple Json Parser in Haskell

## Intro

This is a simple Json Parser implemented in haskell completely from scratch without using anything other than the standard base library in ghc. This is mainly an educational project rather than anything that should be used in production code.

Big thanks to help from [Tsoding's video](https://www.youtube.com/watch?v=N9RUqGYuGfw)!

## Usage

Recommended way is to install nix and then simply run (no need to manually install haskell infrastructure, nix does it for you!):

```bash
# To activate a shell with required tools like ghc, cabal etc.
nix-shell shell.nix

# To build library, tests, documentation
cabal v2-build

# To build and run tests
cabal v2-test --enable-coverage

# Alternative way to run test
cabal v2-run json-parser-haskell-test

# Clean up
cabal v2-clean
```

For building a release in nix store, which will contain the lib and executable only.

```
nix-build nix/release.nix --no-out-link
```
## Todo

- Floating point type parsing
- QuickCheck property based testing

## Appendix
### Project creation setup (only for me)

```
nix-prefetch-git https://github.com/nixos/nixpkgs.git refs/heads/nixpkgs-unstable > ./nix/nixpkgs_pinned.json

nix-shell --pure -p ghc cabal-install --run "cabal init --interactive"

nix-shell --pure -p cabal2nix --run "cabal2nix ." > nix/cabal2nix_generated_derivation.nix
```

## Cabal changes

Update nix derivation and reload env via

```
cd nix && \
cabal2nix .. > ./cabal2nix_generated_derivation.nix && \
cd .. && \
direnv reload
```
