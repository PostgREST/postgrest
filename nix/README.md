# Nix development and build environment

With Nix it's possible to quickly and reliably recreate the full environments
for developing, testing and building PostgREST.

## Getting started with Nix

You'll need to [get Nix](https://nixos.org/download.html). The installer will
create your Nix store in the `/nix/` directory, where all build artifacts and
their dependencies will be stored. It will also link the Nix executables like
`nix-env`, `nix-build` and `nix-shell` into your PATH. Nix will manage all
other PostgREST dependencies from here on out. To clean up older build
artifacts from the `/nix/store`, you can run `nix-collect-garbage`.

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

We recommend that you use the PostgREST binary cache on
[cachix](https://cachix.org/):

```
# Install cachix:
nix-env -iA cachix -f https://cachix.org/api/v1/install

# Set cachix up to use the PostgREST binary cache:
cachix use postgrest

```

Without cachix, your machine will have to rebuild all the dependencies that are
derived on top of `Musl` for the static builds, which can take a very long time.

## Developing

A development environment for PostgREST is available with `nix-shell`. The
following command will put you into a new shell that has GHC and Cabal on the
PATH:

```bash
nix-shell

```

Within `nix-shell`, you can run Cabal commands as usual. You can also run
stack with the `--nix` option, which causes stack to pick up the non-Haskell
dependencies from the same pinned Nixpkgs version that the Nix builds use.

## Aside: Working with `nix-shell` and the PostgREST utility scripts

The PostgREST utilities available in `nix-shell` all have names that begin with
`postgrest-`, so you can use tab completion (typing `postgrest-` and pressing
`<tab>`) in `nix-shell` to see all that are available:

```
# Note: The utilities listed here might not be up to date.
[nix-shell]$ postgrest-<tab>
postgrest-lint                         postgrest-test-spec-postgresql-10
postgrest-style                        postgrest-test-spec-postgresql-11
postgrest-style-check                  postgrest-test-spec-postgresql-12
postgrest-test-all                     postgrest-test-spec-postgresql-9.4
postgrest-test-spec                    postgrest-test-spec-postgresql-9.5
postgrest-test-spec-all                postgrest-test-spec-postgresql-9.6

[nix-shell]$

```

To run one-off commands, you can also use `nix-shell --run <command>`, which
will lauch the Nix shell, run that one command and exit. Note that the tab
completion will not work with `nix-shell --run`, as Nix has yet to evaluate
our Nix expressions to see which utilities are available.

```
$ nix-shell --run postgrest-style

# Note that you need to quote any arguments that you would like to pass to
# the command to be run in nix-shell:
$ nix-shell --run "postgrest-foo --bar"

```

A third option is to install utilities that you use very often locally:

```
$ nix-env -f default.nix -iA style

# `postgrest-style` can now be run directly:
$ postgrest-style

```

Note that this does not yet work for all utilities (e.g. `postgrest-test-spec` currently
needs to be run within the Nix shell environment).

If you use `nix-shell` very often, you might like to use https://github.com/xzfc/cached-nix-shell,
which skips evaluating all our Nix expressions if nothing changed, reducing startup time for the
shell considerably.

## Testing

In nix-shell, you'll find utility scripts that make it very easy to run the
Haskell test suite, including setting up all required dependencies and
temporary test databases:

```bash
# Run the tests against the most recent version of PostgreSQL:
$ nix-shell --run postgrest-test-spec

# Run the tests against all supported versions of PostgreSQL:
$ nix-shell --run postgrest-test-spec-all

# Run the tests against a specific version of PostgreSQL (use tab-completion in
# nix-shell to see all available versions):
$ nix-shell --run postgrest-test-spec-postgresql-9.5.21

```

## Linting and styling code

The nix-shell also contains scripts for linting and styling the PostgREST
source code:

```bash
# Linting
$ nix-shell --run postgrest-lint

# Styling / auto-formatting code
$ nix-shell --run postgrest-style

```

## Tour

The following is not required for working on PostgREST with Nix, but it will
give you some more background and details on how it works.

### `default.nix`

[`default.nix`](../default.nix) is our 'respository expression' that pulls all
the pieces that we define with Nix together. It returns a set (like a dict in
other programming languages), where each attribute is a derivation that Nix
knows how to build, like the `postgrest` attribute from earlier.

Internally, our `default.nix` uses the `pkgs.callPackage` function to import
the modules that we defined in the `nix` directory. It automatically passes the
arguments those modules require if they are available in `pkgs` (this means
that `pkgs` is defined in terms of itself, better not to think too much about
that).

We also use `default.nix` to load our pinned version of the `nixpkgs`
repository. This set of packages will always be the same, independently from
where or when you use it. The pinned version can be upgraded with the small
`nixpkgs-upgrade` utility. Running `nixpkgs-upgrade > nix/nixpkgs-version.nix`
in `nix-shell` will upgrade the pinned version to the latest `nixpkgs-unstable`
version.

### `shell.nix`

[`shell.nix`](../shell.nix) defines an environment in which PostgREST can be
built and developed. It extends the build enviroment from our `postgrest`
attribute with useful utilities that will be put on the PATH in `nix-shell`.

### `nix/overlays`

Our overlays to the Nix package set are defined here. They allow us to tweak our
`pkgs` in `default.nix` by adding new packages or overriding existing ones.
