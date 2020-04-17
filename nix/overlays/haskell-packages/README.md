
The `.nix` files for individual Haskell packages  were generated with
`cabal2nix`, e.g.:

```bash
cabal2nix --no-check cabal://hasql-pool > hasql-pool.nix

```

Those overrides might become obsolete as the builds of the packages are fixed
on nixpkgs.
