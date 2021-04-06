# hsie - Swiss army knife for HaSkell Imports and Exports

This tool parses Haskell source code to analyse the imports and exports in a
project. It's available in PostgREST's `nix-shell` by default.

## Dumping imports

Given source code in the directories `src` and `main`, for example, you can run:

```
hsie dump-imports src main
```

This dumps all imports of the modules in the given directory to a CSV file,
printed on `stdout`.

To dump to a JSON file (e.g., to further process with `jq`), add the `--json`
flag:

```
hsie dump-imports --json src main
```

## Graphing imports

The tool can generate `graphviz` graphs of module and symbol imports by printing
a file to `stdout` that can directly be rendered with `dot`:

```
hsie graph-modules src main | dot -Tpng -o modules.png
```

The command `graph-modules` prints a graph of which modules insert which other
modules. `graph-symbols` shows which symbols are imported from which modules.

## Checking imports

To check whether modules are imported under consistent aliases in your project,
run:

```
hsie check-aliases main src
```

This will exit with a non-zero exit code if any inconsistent aliases are found.

The following command checks whether any modules are imported as wildcards, i.e.
not qualified and without specifying symbols.

```
hsie check-wildcards main src
```

To whitelist certain modules to be imported as wildcards, use `--ok`:

```
hsie check-wildcards main src --ok Protolude --ok Test.Module
```

## Current limitations

This tool uses the GHC parser to parse Haskell source code. Language extensions
required to parse each file are detected based on the `{-# LANGUAGE ... #-}`
pragmas. If they are not available (e.g., as they are listed as default
extensions in the `.cabal` file), parses may fail. We can fix this by using
an extended set of non-conflicting extensions by default, as `hlint` does for
example.
