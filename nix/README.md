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

If you are on a system that does not support nix, for example Windows, you can
run the nix development environment in a docker container. Inside the `nix/`
directory run `docker-compose run --rm nix` to start the docker container. This
will set up the binary cache and launch `nix-shell` automatically.

## Building PostgREST

To build PostgREST from your local checkout of the repository, run:

```bash
nix-build --attr postgrestPackage

```

This will create a `result` directory that contains the PostgREST binary at
`result/bin/postgrest`. The `--attr` parameter (or short: `-A`) tells Nix to
build the `postgrestPackage` attribute from the Nix expression it finds in our
`default.nix` (see below for details). Nix will take care of getting the right
GHC version and all the build dependencies.

## Binary cache

We recommend that you use the PostgREST binary cache on
[cachix](https://cachix.org/):

```bash
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

## Working with `nix-shell` and the PostgREST utility scripts

The PostgREST utilities available in `nix-shell` all have names that begin with
`postgrest-`, so you can use tab completion (typing `postgrest-` and pressing
`<tab>`) in `nix-shell` to see all that are available:

```bash
# Note: The utilities listed here might not be up to date.
[nix-shell]$ postgrest-<tab>
postgrest-build                   postgrest-test-spec
postgrest-check                   postgrest-watch
postgrest-clean                   postgrest-with-all
postgrest-coverage                postgrest-with-postgresql-10
postgrest-lint                    postgrest-with-postgresql-11
postgrest-run                     postgrest-with-postgresql-12
postgrest-style                   postgrest-with-postgresql-13
postgrest-style-check             postgrest-with-postgresql-9.6
postgrest-test-io
...

[nix-shell]$

```

Some additional modules like `memory`, `docker` and `release`
have large dependencies that would need to be built before the shell becomes
available, which could take an especially long time if the cachix binary cache
is not used. You can activate those by passing a flag to `nix-shell` with
`nix-shell --arg <module> true`. This will make the respective utilites available:

```bash
$ nix-shell --arg memory true
[nix-shell]$ postgrest-<tab>
postgrest-build                   postgrest-test-spec
postgrest-check                   postgrest-watch
postgrest-clean                   postgrest-with-all
postgrest-coverage                postgrest-with-postgresql-10
postgrest-lint                    postgrest-with-postgresql-11
postgrest-run                     postgrest-with-postgresql-12
postgrest-style                   postgrest-with-postgresql-13
postgrest-style-check             postgrest-with-postgresql-9.6
postgrest-test-io
postgrest-test-memory
...

```

Note that `postgrest-test-memory` is now also available.

To run one-off commands, you can also use `nix-shell --run <command>`, which
will lauch the Nix shell, run that one command and exit. Note that the tab
completion will not work with `nix-shell --run`, as Nix has yet to evaluate
our Nix expressions to see which utilities are available.

```bash
$ nix-shell --run postgrest-style

# Note that you need to quote any arguments that you would like to pass to
# the command to be run in nix-shell:
$ nix-shell --run "postgrest-foo --bar"

```

A third option is to install utilities that you use very often locally:

```bash
$ nix-env -f default.nix -iA devTools

# `postgrest-style` can now be run directly:
$ postgrest-style

```

If you use `nix-shell` very often, you might like to use
https://github.com/xzfc/cached-nix-shell, which skips evaluating all our Nix
expressions if nothing changed, reducing startup time for the shell
considerably.

Note: Once inside nix-shell, the utilities work from any directory inside
the PostgREST repo. Paths are resolved relative to the repo root:

```bash
$ cd src
# Even though the current directory is ./src, the config path must still start
# from the repo root:
$ postgrest-run test/io/configs/simple.conf
```

## Testing

In nix-shell, you'll find utility scripts that make it very easy to run our
test suite, including setting up all required dependencies and
temporary test databases:

```bash
# Run the tests against the most recent version of PostgreSQL:
$ nix-shell --run postgrest-test-spec

# Run the tests against all supported versions of PostgreSQL:
$ nix-shell --run "postgrest-with-all postgrest-test-spec"

# Run the tests against a specific version of PostgreSQL (use tab-completion in
# nix-shell to see all available versions):
$ nix-shell --run "postgrest-with-postgresql-13 postgrest-test-spec"

```

The io-test that test PostgREST as a black box with inputs and outputs can be
run with `postgrest-test-io`. The test runner under the hood is
[pytest](https://docs.pytest.org/) and you can pass it the usual options:

```bash
# Filter the tests to run by name, including all that contain 'config':
postgrest-test-io -k config

# Run tests in parallel using xdist, specifying the number of processes:
postgrest-test-io -n auto
postgrest-test-io -n 8
```

The memory tests check that we don't surpass a memory threshold for big request bodies.

```bash
# Build the dependencies needed for the memory test
nix-shell --arg memory true

# Run the memory test
postgrest-test-memory
```

The loadtests ensure that performance doesn't drop on a change. Underlyingly they use
[vegeta](https://github.com/tsenart/vegeta).

```bash
# Run the loadtests on the latest commit(HEAD)
postgrest-loadtest

# You can loadtest comparing to a different branch
postgrest-loadtest-against master

# You can simulate latency client/postgrest and postgrest/database
PGRST_DELAY=5ms PGDELAY=5ms postgrest-loadtest

# You can build postgrest directly with cabal for faster iteration
PGRST_BUILD_CABAL=1 postgrest-loadtest

# Produce a markdown report to be used on CI
postgrest-loadtest-report
```

doctests for some of our modules are also available:

```bash
postgrest-test-doctest
```

## Code coverage

Code coverage is available under the `postgrest-coverage` command. This will produce a `./coverage` directory that can be visualized with a simple http server.

```bash
# Will run all the tests and produce a coverage dir
postgrest-coverage

# Visualize the output
cd coverage
python -mSimpleHTTPServer 8080
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

There is also `postgrest-style-check` that exits with a non-zero exit code if
the check resulted in any uncommited changes. It's mostly useful for CI.

## General development tools

Tools like `postgrest-build`, `postgrest-run` etc. are simple wrappers around
`cabal` and should do what you expect. `postgrest-check` runs most checks that will
also run in CI, with the exception of the IO and Memory checks that need to be run
separately.

`postgrest-with-postgresql-*` take a command as an argument and will run it
with a temporary database. `postgrest-with-all` will run the command against
all supported PostgreSQL versions. Tests run without `postgrest-with-*` are
run against the latest PostgreSQL version by default.

`postgrest-watch` takes a command as an argument that it will re-run if any source
file is changed. For example, `postgrest-watch postgrest-with-all postgrest-test-spec`
will re-run the full spec test suite against all PostgreSQL versions on every change.

## Tour

The following is not required for working on PostgREST with Nix, but it will
give you some more background and details on how it works.

### `default.nix`

[`default.nix`](../default.nix) is our 'repository expression' that pulls all
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

## Upgrading dependencies

See the [upgrading checklist](UPGRADE.md) for how to upgrade the PostgREST
dependencies.
