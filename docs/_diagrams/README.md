## ERD

The ER diagrams were created with https://github.com/BurntSushi/erd/.

You can go download erd from https://github.com/BurntSushi/erd/releases and then do:

```bash
./erd_static-x86-64 -i ./er/film.er -o ../_static/film.png
```

The fonts used belong to the GNU FreeFont family. You can download them here: http://ftp.gnu.org/gnu/freefont/

## LaTeX

The schema structure diagram is done with LaTeX. You can use a GUI like https://www.mathcha.io/editor to create the .tex file.

Then use this command to generate the png file.

```bash
postgrest-docs-render
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

## UML

The UML diagrams are created with https://plantuml.com/.

PlantUML only creates one diagram per file.
That's why we need to create another one for dark mode.
For example, for the file [uml/arch.uml](uml/arch.uml) there's [uml/dark/arch-dark.uml](uml/dark/arch-dark.uml) which includes the first one:

```bash
plantuml -tsvg uml/arch.uml -o ../../_static
plantuml -tsvg -darkmode uml/dark/arch-dark.uml -o ../../../_static
```
