{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.PGF.Surface
-- Maintainer  :  c.chalmers@me.com
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
      -- * Predefined surfaces
    , latexSurface
    , contextSurface
    , plaintexSurface
      -- * Lenses
    , texFormat
    , command
    , arguments
    , pageSize
    , preamble
    , beginDoc
    , endDoc
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
  { _texFormat :: TeXFormat -- ^ Format PGF commands use.
  , _command   :: String    -- ^ System command to be called.
  , _arguments :: [String]  -- ^ Auguments for command.
  , _pageSize  :: Maybe ((Int,Int) -> String)
                            -- ^ Command to change page size from dimensions of image.
  , _preamble  :: String    -- ^ Preamble for document, should import pgfcore.
  , _beginDoc  :: String    -- ^ Begin document
  , _endDoc    :: String    -- ^ End document.
  }

makeLenses ''Surface

latexSurface :: Surface
latexSurface = Surface
  { _texFormat = LaTeX
  , _command   = "pdflatex"
  , _arguments = []
  , _pageSize  = Just $ \(w,h) ->
                 "\\pdfpagewidth=" ++ show w ++ "bp\n"
              ++ "\\pdfpageheight=" ++ show h ++ "bp\n"
              ++ "\\pdfhorigin=-76.6bp\n"
              ++ "\\pdfvorigin=-52.8bp"
  , _preamble  = "\\documentclass{article}\n"
              ++ "\\usepackage{pgfcore}\n"
              ++ "\\pagenumbering{gobble}\n"
  , _beginDoc  = "\\begin{document}"
  , _endDoc    = "\\end{document}"
  }

contextSurface :: Surface
contextSurface = Surface
  { _texFormat = ConTeXt
  , _command   = "context"
  , _arguments = ["--pipe", "--once"]
  , _pageSize  = Just $ \(w,h) ->
                 "\\definepapersize[diagram][width="++ show w ++"bp,height="++ show h ++"bp]\n"
              ++ "\\setuppapersize[diagram][diagram]\n"
              ++ "\\setuplayout\n"
              ++ "  [ topspace=0bp\n"
              ++ "  , backspace=0bp\n"
              ++ "  , header=0bp\n"
              ++ "  , footer=0bp\n"
              ++ "  , width=" ++ show w ++ "bp\n"
              ++ "  , height=" ++ show h ++ "bp\n"
              ++ "  ]"
  , _preamble  = "\\usemodule[pgf]\n" -- pgfcore doesn't work
              ++ "\\setuppagenumbering[location=]"
  , _beginDoc  = "\\starttext"
  , _endDoc    = "\\stoptext"
  }

plaintexSurface :: Surface
plaintexSurface = Surface
  { _texFormat = PlainTeX
  , _command   = "pdftex"
  , _arguments = []
  , _pageSize  = Just $ \(w,h) ->
                 "\\pdfpagewidth=" ++ show w ++ "bp\n"
              ++ "\\pdfpageheight=" ++ show h ++ "bp\n"
              ++ "\\pdfhorigin=-20bp\n"
              ++ "\\pdfvorigin=0bp"
  , _preamble  = "\\input eplain\n"
              ++ "\\beginpackages\n\\usepackage{color}\n\\endpackages\n"
              ++ "\\input pgfcore\n"
              ++ "\\def\\frac#1#2{{\\begingroup #1\\endgroup\\over #2}}"
              ++ "\\nopagenumbers"
  , _beginDoc  = ""
  , _endDoc    = "\\bye"
  }

-- | LaTeX is the default surface.
instance Default Surface where
  def = latexSurface

------------------------------------------------------------------------
-- Hashable instances

instance Hashable (TeXFormat) where
  hashWithSalt s LaTeX    = s `hashWithSalt` (1::Int)
  hashWithSalt s ConTeXt  = s `hashWithSalt` (2::Int)
  hashWithSalt s PlainTeX = s `hashWithSalt` (3::Int)

instance Hashable (Surface) where
  hashWithSalt s (Surface tf cm ar ps pr bd ed)
    = s                 `hashWithSalt`
      tf                `hashWithSalt`
      cm                `hashWithSalt`
      ar                `hashWithSalt`
      ps <*> Just (1,2) `hashWithSalt`
      pr                `hashWithSalt`
      bd                `hashWithSalt`
      ed

