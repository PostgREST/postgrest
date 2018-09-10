# PostgREST documentation

PostgREST docs use the reStructuredText format, check this [cheatsheet](https://github.com/ralsina/rst-cheatsheet/blob/master/rst-cheatsheet.rst) to get acquainted with it.

To generate HTML version:

1. Install Sphinx from the [sphinx website](http://www.sphinx-doc.org/en/stable/install.html)
2. Clone this repository
3. Generate HTML
    ```bash
    cd postgrest-docs
    sphinx-build -b html -a -n . _build

    # open _build/index.html in your browser
    ```

If you use [nix](https://nixos.org/nix/), you can just run:

```
  nix-shell
```

This will build the docs and start a livereload server on `http://localhost:5500`.

---

**Sphinx Installation Notes:**

* If you're on OSX you might want to install the Python from homebrew - then a simple `pip install sphinx` does the trick.
