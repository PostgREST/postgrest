# PostgREST documentation https://postgrest.org/

PostgREST docs use the reStructuredText format, check this [cheatsheet](https://github.com/ralsina/rst-cheatsheet/blob/master/rst-cheatsheet.rst) to get acquainted with it.

To build the docs locally, use [nix](https://nixos.org/nix/):

```bash
  nix-shell
```

Once in the nix-shell you have the following commands available:

- `postgrest-docs-build`: Build the docs.
- `postgrest-docs-serve`: Build the docs and start a livereload server on `http://localhost:5500`.
- `postgrest-docs-spellcheck`: Run aspell.

## Documentation structure

This documentation is structured according to tutorials-howtos-topics-references. For more details on the rationale of this structure, 
see https://www.divio.com/blog/documentation.
