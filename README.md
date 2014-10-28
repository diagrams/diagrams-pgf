## PGF diagrams backend

`diagrams-pgf` is a [PGF] backend for [diagrams]. Diagrams is a powerful, flexible, declarative domain-specific language for creating vector graphics, using the [Haskell programming language][haskell].

[PGF]: http://sourceforge.net/projects/pgf/
[diagrams]: http://projects.haskell.org/diagrams/
[haskell]: http://www.haskell.org/haskellwiki/Haskell

PGF is a TeX macro package for generating graphics. It is platform- and format-independent and works together with the most important TeX backend drivers, including pdftex and dvips.

`diagrams-pgf` is a work in progress, it supports the basic features of diagrams with the following features:

- LaTeX, ConTeXt and plain TeX support
- direct PDF generation using a TeX distribution (e.g. [texlive](https://www.tug.org/texlive/)) via [texrunner].

### Usage

A simple example that uses diagrams-pgf to draw a square.

```haskell
import Diagrams.Prelude
import Diagrams.Backend.PGF.CmdLine

b1 = square 20 # lwG 0.05

main = defaultMain (pad 1.1 b1)
```

Save this to file named `square.hs` and compile this program:

```
ghc --make square.hs
```

This will generate an executable which, when run produces an SVG file. Run the
executable with the `--help` option to find out more about how to call it.

```
$ ./square --help
square

Usage: square [-?|--help] [-w|--width WIDTH] [-h|--height HEIGHT]
              [-o|--output OUTPUT] [-f|--format FORMAT] [-a|--standalone]
              [-r|--readable] [-l|--loop] [-s|--src ARG]
              [-i|--interval INTERVAL]
  Command-line diagram generation.

Available options:
  -?,--help                Show this help text
  -w,--width WIDTH         Desired WIDTH of the output image
  -h,--height HEIGHT       Desired HEIGHT of the output image
  -o,--output OUTPUT       OUTPUT file
  -f,--format FORMAT       l for LaTeX, c for ConTeXt, p for plain
                           TeX (default: LaTeX)
  -a,--standalone          Produce standalone .tex output
  -r,--readable            Indent lines
  -l,--loop                Run in a self-recompiling loop
  -s,--src ARG             Source file to watch
  -i,--interval INTERVAL   When running in a loop, check for changes every
                           INTERVAL seconds.
```

If no output file is given, output is send to `stdout`. Supported outputs are `.tex` and `.pdf`. PDF generation is done using [texrunner].

[texrunner]: http://www.github.com/cchalmers/texrunner

```
$ ./Square -o square.tex
$ cat ./square.tex
\begin{pgfpicture}
  \pgfpathrectangle{\pgfpointorigin}{\pgfqpoint{22.0000px}{22.0000px}}
  \pgfusepath{use as bounding box}
  \begin{pgfscope}
    \begin{pgfscope}
      \pgfpathmoveto{\pgfqpoint{21.0000px}{1.0000px}}
      \pgfpathlineto{\pgfqpoint{21.0000px}{21.0000px}}
      \pgfpathlineto{\pgfqpoint{1.0000px}{21.0000px}}
      \pgfpathlineto{\pgfqpoint{1.0000px}{1.0000px}}
      \pgfpathlineto{\pgfqpoint{21.0000px}{1.0000px}}
      \pgfpathclose
      \definecolor{sc}{rgb}{0.0000,0.0000,0.0000}
      \pgfsetstrokecolor{sc}
      \pgfsetlinewidth{0.0132mm}
      \pgfusepath{stroke}
    \end{pgfscope}
  \end{pgfscope}
\end{pgfpicture}
```

### Typesetting

`pgf-diagrams` allows typesetting TeX commands and can calculate the corresponding envelope. See the [hbox example] to see how get envelopes of text:

[hbox example]: examples/hbox.hs

![hbox](https://rawgit.com/cchalmers/texrunner/master/diagrams/hbox.svg)


### Missing features

The following features are not currently supported:

- selecting fonts (italic and bold work)
- gradients

