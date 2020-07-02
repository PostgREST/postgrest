The `.nix` files in this directory pin specific versions of Haskell packages
from Hackage. They were generated with `cabal2nix`, e.g.:

```bash
cabal2nix cabal://Cabal-3.0.0.0 > Cabal.nix

```

Those overrides will likely become obsolete as the pinned versions are adopted
into our pinned version of nixpkgs.
