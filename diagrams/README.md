## ERD

The ER diagrams were created with https://github.com/BurntSushi/erd/.

You can go download erd from https://github.com/BurntSushi/erd/releases and then do:

```bash
./erd_static-x86-64 -i diagrams/film.er -o _static/film.png
```

## LaTeX

The schema structure diagram is done with LaTeX. You can use a GUI like https://www.mathcha.io/editor to create the .tex file.

Then use this command to generate the png file.

```bash
pdflatex --shell-escape -halt-on-error db.tex

## and move it to the static folder(it's not easy to do it in one go with the pdflatex)
mv db.png ../_static/
```

LaTeX is used because it's a tweakable plain text format.

You can install the full latex suite with `nix`:

```
nix-env -iA texlive.combined.scheme-full
```

To tweak the file with a live reload environment use:

```bash
# open the pdf(zathura used as an example)
zathura db.pdf &

# live reload with entr
echo db.tex | entr pdflatex --shell-escape -halt-on-error db.tex
```
