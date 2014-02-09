_diagrams-pfg_ is a [PGF] backend for [diagrams]. Diagrams is a powerful, flexible, declarative domain-specific language for creating vector graphics, using the [Haskell programming language][haskell].

[PGF]:http://sourceforge.net/projects/pgf/
[diagrams]: http://projects.haskell.org/diagrams/
[haskell]: http://www.haskell.org/haskellwiki/Haskell

PGF is a TeX macro package for generating graphics. It is platform- and format-independent and works together with the most important TeX backend drivers, including pdftex and dvips.

_diagrams-pgf_ is a work in progress, it supports the basic features of diagrams with the following features:

- LaTeX, ConTeXt and plain TeX support
- direct PDF generation using _pdflatex_, _context_ and _pdftex_

# Usage

A simple example that uses _diagrams-pgf_ to draw a square.

```haskell
import Diagrams.Prelude
import Diagrams.Backend.PGF.CmdLine

b1 = square 20 # lw 0.002

main = defaultMain (pad 1.1 b1)
```

Save this to file named `Square.hs` and compile this program:

```
ghc --make Square.hs
```

This will generate an executable which, when run produces an SVG file. Run the
executable with the `--help` option to find out more about how to call it.

```
$ ./Square --help
Square

Usage: Square [-w|--width WIDTH] [-h|--height HEIGHT] [-o|--output OUTPUT] [-f|--format FORMAT] [-l|--loop] [-s|--src ARG] [-i|--interval INTERVAL]
  Command-line diagram generation.

Available options:
  -?,--help                Show this help text
  -w,--width WIDTH         Desired WIDTH of the output image
  -h,--height HEIGHT       Desired HEIGHT of the output image
  -o,--output OUTPUT       OUTPUT file
  -f,--format FORMAT       l for LaTeX, c for ConTeXt, p for plain TeX (default: LaTeX)
  -l,--loop                Run in a self-recompiling loop
  -s,--src ARG             Source file to watch
  -i,--interval INTERVAL   When running in a loop, check for changes every INTERVAL seconds.
```

If no output file is given, output is send to `stdout`. Supported outputs are ".tex" and ".pdf". The tex compiler is shelled out for pdf output.

```
$ ./Square -o square.tex
$ cat ./square.tex
\begin{pgfpicture}
\pgfpathrectangle{\pgfpointorigin}{\pgfqpoint{22.0000px}{22.0000px}}
\pgfusepath{use as bounding box}
  \begin{pgfscope}
    \pgfpathmoveto{\pgfqpoint{21.0000px}{1.0000px}}
    \pgfpathlineto{\pgfqpoint{21.0000px}{21.0000px}}
    \pgfpathlineto{\pgfqpoint{1.0000px}{21.0000px}}
    \pgfpathlineto{\pgfqpoint{1.0000px}{1.0000px}}
    \pgfpathlineto{\pgfqpoint{21.0000px}{1.0000px}}
    \pgfpathclose
    \definecolor{fc}{rgb}{0.0000,0.0000,0.0000}
    \pgfsetfillcolor{fc}
    \pgfsetlinewidth{0.0020cm}
    \pgfusepath{stroke}
  \end{pgfscope}
\end{pgfpicture}
```

## Missing feature / Shortcomings

The following features are not currently supported:

- freezing
- images
- text alignment (only extreme cases supported)
- selecting fonts (italic and bold work)

Other features seems to be working OK. (see [tests])

TeX's text typesetting is one of the major advantages of using PGF. Currently there is little support (text is passed unescaped), although there is an option to disable transformations to text.

[tests]:http://github.com/cchalmers/diagrams-backend-tests
