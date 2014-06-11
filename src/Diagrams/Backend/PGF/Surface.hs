{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
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

import Control.Applicative
import Control.Lens        (makeLenses)
import Data.Default        (Default (..))
import Data.Hashable       (Hashable (..))
import Data.Typeable       (Typeable)

-- | The 'TeXFormat' is used to choose the different PGF commands nessesary for
--   that format.
data TeXFormat = LaTeX | ConTeXt | PlainTeX
  deriving (Show, Read, Eq, Typeable)

data Surface = Surface
  { _texFormat :: TeXFormat          -- ^ Format PGF commands use.
  , _command   :: String             -- ^ System command to be called.
  , _arguments :: [String]           -- ^ Auguments for command.
  , _jobArg    :: String -> String   -- ^ Command for specifying jobname.
  , _pageSize  :: Maybe ((Double,Double) -> String)
  , _preamble  :: String             -- ^ Preamble for document, should import pgfcore.
  , _beginDoc  :: String             -- ^ Begin document
  , _endDoc    :: String             -- ^ End document.
  , _pdfOrigin :: Maybe (Double,Double) -- ^ Set origin of picture with
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
  , _pageSize  = Nothing
  , _preamble  = "\\documentclass{article}\n"
              ++ "\\usepackage{pgfcore}\n"
              ++ "\\pagenumbering{gobble}\n"
  , _beginDoc  = "\\begin{document}"
  , _endDoc    = "\\end{document}"
  , _pdfOrigin = Just (-76.7, -52.8)
  }

contextSurface :: Surface
contextSurface = Surface
  { _texFormat = ConTeXt
  , _command   = "context"
  , _arguments = ["--pipe"]
  , _jobArg    = \jobname -> "--dummyfile"++jobname
  , _pageSize  = Just $ \(w,h) ->
                 "\\definepapersize[diagram][width="++show w++"bp,height="++show h++"bp]\n"
              ++ "\\setuppapersize[diagram][diagram]\n"
              ++ "\\setuplayout\n"
              ++ "  [ topspace=0bp\n"
              ++ "  , backspace=0bp\n"
              ++ "  , header=0bp\n"
              ++ "  , footer=0bp\n"
              ++ "  , width=" ++ show (ceiling w :: Int) ++ "bp\n"
              ++ "  , height=" ++ show (ceiling h :: Int) ++ "bp\n"
              ++ "  ]"
  , _preamble  = "\\usemodule[pgf]\n" -- pgfcore doesn't work
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
  , _preamble  = "\\input eplain\n"
              ++ "\\beginpackages\n\\usepackage{color}\n\\endpackages\n"
              ++ "\\input pgfcore\n"
              ++ "\\def\\frac#1#2{{\\begingroup #1\\endgroup\\over #2}}"
  , _beginDoc  = ""
  , _endDoc    = "\\bye"
  , _pdfOrigin = Just (-20, 0)
  }

instance Default Surface where
  def = latexSurface

------------------------------------------------------------------------
-- Hashable instances

instance Hashable (TeXFormat) where
  hashWithSalt s LaTeX    = s `hashWithSalt` (1::Int)
  hashWithSalt s ConTeXt  = s `hashWithSalt` (2::Int)
  hashWithSalt s PlainTeX = s `hashWithSalt` (3::Int)

instance Hashable (Surface) where
  hashWithSalt s (Surface tf cm ar ja ps pr bd ed o)
    = s                 `hashWithSalt`
      tf                `hashWithSalt`
      cm                `hashWithSalt`
      ar                `hashWithSalt`
      ja "job"          `hashWithSalt`
      ps <*> Just (1,2) `hashWithSalt`
      pr                `hashWithSalt`
      bd                `hashWithSalt`
      ed                `hashWithSalt`
      o

