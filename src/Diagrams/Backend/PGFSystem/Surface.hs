-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.PGF.Surface
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A 'Surface' defines how a pgfpicture should be placed and compiled. Surfaces 
-- are used for rendering a @.tex@ or @.pdf@ using functions from 
-- 'Diagrams.Backend.PGF'.
--
-- Surfaces are also used in 'Diagrams.Backend.PGF.Typeset' for quereying 
-- envelopes of text.
--
-- Surfaces for LaTeX, ConTeXt and plain TeX are provided and reexported by 
-- Diagrams.Backend.PGF, but can be adjusted here as required.
-----------------------------------------------------------------------------

module Diagrams.Backend.PGFSystem.Surface
    ( -- * Surface definition
      Surface(..)
    , TeXFormat(..)
    , texFormat
    , command
    , arguments
    , jobArg
    , pageSize
    , preamble
    , beginDoc
    , endDoc
    , pdfOrigin
      -- * Predefined surfaces
    , latexSurface
    , contextSurface
    , plaintexSurface
    ) where

import Diagrams.Backend.PGF.Surface hiding (latexSurface, contextSurface,
                                            plaintexSurface)

latexSurface :: Surface
latexSurface = Surface
  { _texFormat = LaTeX
  , _command   = "pdflatex"
  , _arguments = []
  , _jobArg    = \jobname -> "-jobname="++jobname
  , _pageSize  = Nothing
  , _preamble  = "\\catcode`\\@=11\n\\documentclass{article}\n\\usepackage{pgfsys}"
  , _beginDoc  = "\\begin{document}"
  , _endDoc    = "\\end{document}"
  , _pdfOrigin = Just (-2.712, -1.85)
  }

contextSurface :: Surface
contextSurface = Surface
  { _texFormat = ConTeXt
  , _command   = "context"
  , _arguments = ["--pipe"]
  , _jobArg    = \jobname -> "--dummyfile"++jobname
  , _pageSize  = Just $ \(w,h) ->
                 "\\definepapersize[diagram][width="++show w++"px,height="++show h++"px]\n"
              ++ "\\setuppapersize[diagram][diagram]\n"
              ++ "\\setuplayout"
              ++ "[ topspace="++show h++"px"
              ++ ", backspace=0px"
              ++ ", header=0px"
              ++ ", footer=0px"
              ++ ", width="++show w++"px"
              ++ ", height="++show h++"px"
              ++ "]"
  , _preamble  = "\\catcode`\\@=11\n"
              ++ "\\usemodule[pgfsys]\n"
              ++ "\\setuppagenumbering[location=]"
  , _beginDoc  = "\\starttext"
  , _endDoc    = "\\stoptext"
  , _pdfOrigin = Nothing
  }

plaintexSurface :: Surface
plaintexSurface = Surface
  { _texFormat = PlainTeX
  , _command   = "pdftex"
  , _arguments = []
  , _jobArg    = \jobname -> "-jobname="++jobname
  , _pageSize  = Nothing
  , _preamble  = "\\catcode`\\@=11\n"
              ++ "\\input pgfsys\n"
              ++ "\\def\\frac#1#2{{\\begingroup #1\\endgroup\\over #2}}"
  , _beginDoc  = ""
  , _endDoc    = "\\bye"
  , _pdfOrigin = Just (-0.712, 0.02)
  }
