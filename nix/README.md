# Nix development and build environment

Nix helps us to quickly and reliably recreate the full environments for
developing, testing and building PostgREST.

## Getting started with Nix

You'll need to [get Nix](https://nixos.org/download.html). The installer will
create your Nix store in the `/nix/` directory, where all build artifacts and
their dependencies will be stored. It will also link the Nix executables like
`nix-env` and `nix-shell` into your PATH. Nix will manage all other PostgREST
dependencies from here on out. To clean up older build artifacts from the
`/nix/store`, you can run `nix-collect-garbage`.

## Building PostgREST

To build PostgREST from your local checkout of the repository, run:

```bash
nix-build --attr postgrest

```

This will create a `result` directory that contains the PostgREST binary at
`result/bin/postgrest`. The `--attr` parameter (or short: `-A`) tells Nix to
build the `postgrest` attribute from the Nix expression it finds in our
`default.nix` (see below for details). Nix will take care of getting the right
GHC version and all the build dependencies.

## Developing

A full development environment for PostgREST is available with `nix-shell`. The
following command will put you into a new shell that has Cabal, stack and all
the PostgREST linting and testing tools on the PATH:

```bash
nix-shell

```

Within `nix-shell`, you can run cabal commands as usual. You can also use
some utility scripts that are specific to PostgREST:

```bash
postgrest-test-spec # Run the Haskell test suite
postgrest-test-spec-all # Run the Haskell test suite for all PostgreSQL versions
postgrest-test-spec-postgresql-and-plugins-12.12 # Test a specific version

postgrest-test-io # Run end-to-end tests
postgrest-test-memory # Run memory usage tests

postgrest-lint # Run all linters like hlint
postgrest-style # Auto-format code, including Haskell and Nix files

```

All the dependencies of those utility scripts, like several different PostgreSQL
versions, hlint and stylish-haskell, are automatically handled by Nix.
`nix-shell` will add items to your `/nix/store` (which can be cleaned up with
`nix-collect-garbage`), but it will not make any other permanent changes to
your machine or environment.

Running `nix-shell --pure` will put you into a temporary enviroment that
is as independent as possible from you local machine. For example, it will
only have binaries on the PATH that were defined through Nix. If the test
suite passes in `--pure`, you can be pretty sure that all dependencies are
fully defined and that it will pass on any other machine.

To run commands directly, you can also use `--run`:

```bash
nix-shell --pure --run postgrest-test-spec
nix-shell --run "cabal v2-build"

```

## Tour

The following is not required for working on PostgREST with Nix, but it will
give you some more background and details on how it works.

### `default.nix`

[`default.nix`](../default.nix) is our 'respository expression' that pulls all
the pieces that we define with Nix together. It returns a set (like a dict in
other programming languages), where each attribute is a derivation that Nix
knows how to build.  We saw the `postgrest` attribute earlier, but there are
also `tests`, `style`, `lint` etc.

Internally, our `default.nix` uses the `pkgs.callPackage` function to import
the modules that we defined in the `nix` directory. It automatically passes the
arguments those modules require if they are available in `pkgs` (that means
that `pkgs` is defined in terms of itself, better not to think too much about
that... :-) ).

We also use `default.nix` to load our pinned version of the `nixpkgs`
repository. This set of packages will always be the same, independently from
where or when you use it.

### `shell.nix`

[`shell.nix`](../shell.nix) defines an environment in which PostgREST can be
built and developed. It extends the build enviroment from our `postgrest`
attribute with useful utilities that will be put on the PATH in `nix-shell`.

### `nix/overlays`

Our overlays to the Nix package set are defined here. They allow us to tweak our
`pkgs` in `default.nix` by adding new packages or overriding existing ones.
