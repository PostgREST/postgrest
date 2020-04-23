The `.nix` files in this directory pin specific versions of Haskell packages
from Hackage. They were generated with `cabal2nix`, e.g.:

```bash
cabal2nix cabal://protolude > protolude.nix

```

Those overrides might become obsolete as the pinned versions are adopted into
our pinned version of nixpkgs.
