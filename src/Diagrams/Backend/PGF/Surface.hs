{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.PGF.Surface
-- Copyright   :  (c) 2015 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A 'Surface' defines how a pgfpicture should be placed and compiled. Surfaces
-- are used for rendering a @.tex@ or @.pdf@ using functions from
-- 'Diagrams.Backend.PGF'.
--
-- Surfaces are also used in 'Diagrams.Backend.PGF.Hbox' for querying
-- envelopes of text.
--
-- Surfaces for LaTeX, ConTeXt and plain TeX are provided and reexported by
-- Diagrams.Backend.PGF. Lenses here allow these to be adjusted as required.
-----------------------------------------------------------------------------

module Diagrams.Backend.PGF.Surface
  ( -- * Surface definition
    Surface(..)
  , TeXFormat(..)

    -- * Online rendering with surfaces
  , surfOnlineTex
  , surfOnlineTexIO

    -- * Predefined surfaces
  , latexSurface
  , contextSurface
  , plaintexSurface
  , sampleSurfaceOutput

    -- * Lenses
  , texFormat
  , command
  , arguments
  , pageSize
  , preamble
  , beginDoc
  , endDoc
  ) where

import           Data.ByteString.Builder
import           Data.Hashable           (Hashable (..))
import           Data.Typeable           (Typeable)
import           System.IO.Unsafe
import           System.TeXRunner.Online


import           Diagrams.Prelude
import           Prelude

-- | The 'TeXFormat' is used to choose the different PGF commands nessesary for
--   that format.
data TeXFormat = LaTeX | ConTeXt | PlainTeX
  deriving (Show, Read, Eq, Typeable)

data Surface = Surface
  { _texFormat :: TeXFormat -- ^ Format for the PGF commands
  , _command   :: String    -- ^ System command to be called.
  , _arguments :: [String]  -- ^ Auguments for command.
  , _pageSize  :: Maybe (V2 Int -> String)
                            -- ^ Command to change page size from dimensions of image.
                            --   (in bp)
  , _preamble  :: String    -- ^ Preamble for document, should import pgfcore.
  , _beginDoc  :: String    -- ^ Begin document.
  , _endDoc    :: String    -- ^ End document.
  }

makeLensesWith (lensRules & generateSignatures .~ False) ''Surface

-- | Format for the PGF commands.
texFormat :: Lens' Surface TeXFormat

-- | System command to call for rendering PDFs for 'OnlineTeX'.
command :: Lens' Surface String

-- | List of arguments for the 'command'.
arguments :: Lens' Surface [String]

-- | Preamble for the tex document. This should at least import
--   @pgfcore@.
preamble :: Lens' Surface String

-- | Specify the page size for the tex file.
pageSize :: Lens' Surface (Maybe (V2 Int -> String))

-- | Command to begin the document. (This normally doesn't need to
--   change)
beginDoc :: Lens' Surface String

-- | Command to end the document. (This normally doesn't need to
--   change)
endDoc :: Lens' Surface String

-- Predefined surfaces -------------------------------------------------

-- | Default surface for latex files by calling @pdflatex@.
--
-- ==== __Sample output__
--
-- @
-- 'command': pdflatex
--
-- % 'preamble'
-- \documentclass{article}
-- \usepackage{pgfcore}
-- \pagenumbering{gobble}
--
-- % 'pageSize'
-- \pdfpagewidth=100bp
-- \pdfpageheight=80bp
-- \textheight=80bp
-- \pdfhorigin=-76.6bp
-- \pdfvorigin=-52.8bp
--
-- % 'beginDoc'
-- \begin{document}
--
-- \<LaTeX pgf code\>
--
-- % 'endDoc'
-- \end{document}
-- @
--
latexSurface :: Surface
latexSurface = Surface
  { _texFormat = LaTeX
  , _command   = "pdflatex"
  , _arguments = []
  , _pageSize  = Just $ \(V2 w h) ->
                 "\\pdfpagewidth=" ++ show w ++ "bp\n"
              ++ "\\pdfpageheight=" ++ show h ++ "bp\n"
              ++ "\\textheight=" ++ show h ++ "bp\n"
              ++ "\\pdfhorigin=-76.6bp\n"
              ++ "\\pdfvorigin=-52.8bp"
  , _preamble  = "\\documentclass{article}\n"
              ++ "\\usepackage{pgfcore}\n"
              ++ "\\pagenumbering{gobble}"
  , _beginDoc  = "\\begin{document}"
  , _endDoc    = "\\end{document}"
  }

-- | Default surface for latex files by calling @pdflatex@.
--
-- ==== __Sample output__
--
-- @
-- 'command': context --pipe --once

-- % 'preamble'
-- \usemodule[pgf]
-- \setuppagenumbering[location=]
--
-- % 'pageSize'
-- \definepapersize[diagram][width=100bp,height=80bp]
-- \setuppapersize[diagram][diagram]
-- \setuplayout
--   [ topspace=0bp
--   , backspace=0bp
--   , header=0bp
--   , footer=0bp
--   , width=100bp
--   , height=80bp
--   ]
--
-- % 'beginDoc'
-- \starttext
--
-- \<ConTeXt pgf code\>
--
-- % 'endDoc'
-- \stoptext
-- @
--
contextSurface :: Surface
contextSurface = Surface
  { _texFormat = ConTeXt
  , _command   = "context"
  , _arguments = ["--pipe", "--once"]
  , _pageSize  = Just $ \(V2 w h) ->
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

-- | Default surface for latex files by calling @pdflatex@.
--
-- ==== __Sample output__
--
-- @
-- 'command': pdftex
--
-- % 'preamble'
-- \input eplain
-- \beginpackages
-- \usepackage{color}
-- \endpackages
-- \input pgfcore
-- \def\frac#1#2{{\begingroup #1\endgroup\over #2}}\nopagenumbers
--
-- % 'pageSize'
-- \pdfpagewidth=100bp
-- \pdfpageheight=80bp
-- \pdfhorigin=-20bp
-- \pdfvorigin=0bp
--
-- % 'beginDoc'
--
--
-- <PlainTeX pgf code>
--
-- % 'endDoc'
-- \bye
-- @
--
plaintexSurface :: Surface
plaintexSurface = Surface
  { _texFormat = PlainTeX
  , _command   = "pdftex"
  , _arguments = []
  , _pageSize  = Just $ \(V2 w h) ->
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

sampleSurfaceOutput :: Surface -> String
sampleSurfaceOutput surf = unlines
  [ "command: " ++ surf ^. command ++ " " ++ unwords (surf ^. arguments)
  , "\n% preamble"
  , surf ^. preamble
  , "\n% pageSize"
  , view _Just $ surf ^. pageSize <*> Just (V2 100 80)
  , "\n% beginDoc"
  , surf ^. beginDoc
  , "\n<" ++ show (surf ^. texFormat) ++ " pgf code>"
  , "\n% endDoc"
  , surf ^. endDoc
  ]

-- OnlineTeX functions -------------------------------------------------

-- | Get the result of an OnlineTeX using the given surface.
surfOnlineTex :: Surface -> OnlineTeX a -> a
surfOnlineTex surf a = unsafePerformIO (surfOnlineTexIO surf a)
{-# NOINLINE surfOnlineTex #-}

-- | Get the result of an OnlineTeX using the given surface.
surfOnlineTexIO :: Surface -> OnlineTeX a -> IO a
surfOnlineTexIO surf = runOnlineTex (surf^.command) (surf^.arguments) begin
  where
    begin = view strict . toLazyByteString . stringUtf8
          $ surf ^. (preamble <> beginDoc)

-- Hashable instances --------------------------------------------------

instance Hashable TeXFormat where
  hashWithSalt s LaTeX    = s `hashWithSalt` (1::Int)
  hashWithSalt s ConTeXt  = s `hashWithSalt` (2::Int)
  hashWithSalt s PlainTeX = s `hashWithSalt` (3::Int)

instance Hashable Surface where
  hashWithSalt s (Surface tf cm ar ps p bd ed)
    = s                    `hashWithSalt`
      tf                   `hashWithSalt`
      cm                   `hashWithSalt`
      ar                   `hashWithSalt`
      ps <*> Just (V2 1 2) `hashWithSalt`
      p                    `hashWithSalt`
      bd                   `hashWithSalt`
      ed

