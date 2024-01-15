# Checklist for upgrading Nix dependencies

The Nix dependencies of PostgREST should be updated regularly, in most cases it
should be a very simple operation.

```bash
# Update pinned version of Nixpkgs
nix-shell --run postgrest-nixpkgs-upgrade

# Verify that everything builds
nix-build
```

The following checklist guides you through the complete process in more detail.

## Upgrade the pinned version of `nixpkgs`

The pinned version of [`nixpkgs`](https://github.com/NixOS/nixpkgs) is defined
in [`nix/nixpkgs-version.nix`](nixpkgs-version.nix). The pin refers directly to
a GitHub tarball for the given revision, which is more efficient than pulling
the complete Git repository. To upgrade it to the current `main` of
`nixpkgs`, you can use a small utility script defined in
[`nix/nixpkgs-update.nix`](nixpkgs-update.nix):

```bash
# From the root of the repository, enter nix-shell
nix-shell

# Run the utility script to pin the latest revision in main
postgrest-nixpkgs-upgrade

# Exit the nix-shell with Ctrl-d

```

## Review overlays

Check whether the individual [overlays](overlays) are still required.

## Check if patches are still required and update them as needed

We track a number of PostgREST-specific patches in [`nix/patches`](patches).
Check whether the pull-requests/issues linked in the
[`default.nix`](patches/default.nix) have progressed and remove/modify the
patches if they did. If conflicting changes occurred, you might have to rebase
the respective patches.

## Build everything

Using the PostgREST binary Nix cache is recommended. Install
[Cachix](https://cachix.org/) and run `cachix use postgrest`.

Run `nix-build` in the root directory of the project to build all PostgREST
artifacts. This might take a long time, e.g. when our static GHC version needs
to be rebuilt due to changes to some underlying package. If there are any
errors, this is probably due to one of our patches. Try to fix them and re-run
`nix-build` until everything builds.

## Update the PostgREST binary cache

If you have access to the PostgREST cachix signing key, you can push the
artifacts that you built locally to the binary cache. This will accelerate the
CI builds and tests, sometimes dramatically. This might sometimes even be
required to avoid build timeouts in CI.

You'll need to set the `CACHIX_SIGNING_KEY` before proceeding, e.g. by creating
a file containing `export CACHIX_SIGNING_KEY=...` and sourcing that file, which
avoids having the secret in your shell history.

To push all new artifacts to Cachix, run:

```
nix-store -qR --include-outputs $$(nix-instantiate) | cachix push postgrest

# Or, equivalently
nix-shell --run postgrest-push-cachix

```

The `nix-store` command will query the nix-store to list all dependencies and
build artifacts of PostgREST. The `cachix` command will efficiently push
everything that is not yet cached to the binary cache.
