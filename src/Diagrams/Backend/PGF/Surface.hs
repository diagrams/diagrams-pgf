{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
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

module Diagrams.Backend.PGF.Surface
    ( -- * Surface definition
      Surface(..)
    , TeXFormat(..)
    , texFormat
    , command
    , arguments
    , jobArg
    , preamble
    , beginDoc
    , endDoc
    , pdfOrigin
      -- * Predefined surfaces
    , latexSurface
    , contextSurface
    , plaintexSurface
    ) where

import Control.Lens (makeLenses)
import Data.Default
import Data.Typeable

-- | The 'TeXFormat' is used to choose the different PGF commands nessesary for 
--   that format.
data TeXFormat = LaTeX | ConTeXt | PlainTeX
  deriving (Show, Read, Eq, Typeable)

data Surface = Surface
  { _texFormat  :: TeXFormat          -- ^ Format PGF commands use.
  , _command    :: String             -- ^ System command to be called.
  , _arguments  :: [String]           -- ^ Auguments for command.
  , _jobArg     :: String -> String   -- ^ Command for specifying jobname.
  , _preamble   :: String             -- ^ Preamble for document, should import pgfcore.
  , _beginDoc   :: String             -- ^ Begin document
  , _endDoc     :: String             -- ^ End document.
  , _pdfOrigin  :: Maybe (Double,Double) -- ^ Set origin of picture with 
                                         -- \pdfhorigin, \pdfvorigin, if you 
                                         -- don't want the PDF to be cropped, 
                                         -- set nothing.
  }

makeLenses ''Surface

latexSurface :: Surface
latexSurface = Surface
  { _texFormat = LaTeX
  , _command   = "pdflatex"
  , _arguments = []
  , _jobArg    = \jobname -> "-jobname="++jobname
  , _preamble  = "\\documentclass{article}\n\\usepackage{pgfcore}"
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
  , _preamble  = "\\usepackage[pgf]" -- pgfcore doesn't work
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
  , _preamble  = "\\input eplain\n"
              ++ "\\beginpackages\n\\usepackage{color}\n\\endpackages\n"
              ++ "\\input pgfcore"
  , _beginDoc  = ""
  , _endDoc    = "\\bye"
  , _pdfOrigin = Just (-0.712, 0.02)
  }

instance Default Surface where
  def = latexSurface

