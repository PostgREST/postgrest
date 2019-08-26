# PostgREST documentation http://postgrest.org/

PostgREST docs use the reStructuredText format, check this [cheatsheet](https://github.com/ralsina/rst-cheatsheet/blob/master/rst-cheatsheet.rst) to get acquainted with it.

You can use [pipenv](https://pipenv.readthedocs.io) to build the docs locally:

```bash
  pipenv install
  pipenv run python livereload_docs.py
```

Or if you use [nix](https://nixos.org/nix/), you can just run:

```bash
  nix-shell
```

Both of these options will build the docs and start a livereload server on `http://localhost:5500`.

## Documentation structure

This documentation is structured according to tutorials-howtos-topics-references. For more details on the rationale of this structure, 
see https://www.divio.com/blog/documentation.

## Translations

Translations are maintained in separate repositories forked from this one. Once you finish translating in your fork you can upload the project
to https://readthedocs.org and we'll link to it in the official documentation site https://postgrest.org.

See more details in the chinese translation [PR](https://github.com/PostgREST/postgrest-docs/issues/66#issuecomment-297431688).

### Available translations

- Chinese - https://github.com/Lellansin/postgrest-docs (latest version `v0.4.2.0`)
