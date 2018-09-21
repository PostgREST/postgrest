# PostgREST documentation

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
