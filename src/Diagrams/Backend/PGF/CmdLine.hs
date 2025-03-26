{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.PGF.CmdLine
-- Copyright   :  (c) 2015 Diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenient creation of command-line-driven executables for
-- rendering diagrams using the PGF backend.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.PGF.CmdLine
  ( -- * General form of @main@
    -- $mainwith

    mainWith

    -- * Supported forms of @main@

  , defaultMain
  , mainWithSurf
  , onlineMain
  , onlineMainWithSurf
  , multiMain

  , module Diagrams.Backend.PGF
  ) where

import           Data.ByteString.Builder
import           Options.Applicative          as OP

import           System.IO                    (stdout)

import           Diagrams.Backend.CmdLine
import           Diagrams.Prelude             hiding (height, interval, output,
                                               width)

import           Diagrams.Backend.PGF
import           Diagrams.Backend.PGF.Surface

-- pgf specific stuff

data PGFCmdLineOpts = PGFCmdLineOpts
  { _cmdStandalone :: Bool
  , _cmdReadable   :: Bool
  }

makeLenses ''PGFCmdLineOpts

instance Parseable PGFCmdLineOpts where
  parser = PGFCmdLineOpts
        <$> switch
            ( long "standalone"
           <> short 'a'
           <> help "Produce standalone .tex output"
            )
        <*> switch
            ( long "readable"
           <> short 'r'
           <> help "Indent lines"
            )

-- not sure if this is of any use
instance ToResult d => ToResult (OnlineTex d) where
  type Args (OnlineTex d) = (Surface, Args d)
  type ResultOf (OnlineTex d) = IO (ResultOf d)

  toResult d (surf, args) = flip toResult args <$> surfOnlineTexIO surf d

-- $mainwith
-- The 'mainWith' method unifies all of the other forms of @main@ and is
-- now the recommended way to build a command-line diagrams program.  It
-- works as a direct replacement for 'defaultMain' or 'multiMain' as well
-- as allowing more general arguments.  For example, given a function that
-- produces a diagram when given an @Int@ and a @'Colour' Double@, 'mainWith'
-- will produce a program that looks for additional number and color arguments.
--
-- > ... definitions ...
-- > f :: Int -> Colour Double -> Diagram PGF
-- > f i c = ...
-- >
-- > main = mainWith f
--
-- We can run this program as follows:
--
-- > $ ghc --make mydiagram
-- >
-- > # output image.tex built by `f 20 red`
-- > $ ./MyDiagram -o image.tex -w 200 20 red


-- | This is the simplest way to render diagrams, and is intended to
--   be used like so:
--
-- > ... definitions ...
-- >
-- > main = defaultMain myDiagram
--
--   Compiling this file will result in an executable which takes
--   various command-line options for setting the size, output file,
--   and so on, and renders @myDiagram@ with the specified options.
--
--   Pass @--help@ to the generated executable to see all available
--   options. Currently it looks something like
--
-- @
-- mydiagram
--
-- Usage: mydiagram [-?|--help] [-w|--width WIDTH] [-h|--height HEIGHT]
--                  [-o|--output OUTPUT] [-f|--format FORMAT] [-a|--standalone]
--                  [-r|--readable] [-l|--loop] [-s|--src ARG]
--                  [-i|--interval INTERVAL]
--   Command-line diagram generation.
--
-- Available options:
--   -?,--help                Show this help text
--   -w,--width WIDTH         Desired WIDTH of the output image
--   -h,--height HEIGHT       Desired HEIGHT of the output image
--   -o,--output OUTPUT       OUTPUT file
--   -f,--format FORMAT       l for LaTeX, c for ConTeXt, p for plain
--                            TeX (default: LaTeX)
--   -a,--standalone          Produce standalone .tex output
--   -r,--readable            Indent lines
--   -l,--loop                Run in a self-recompiling loop
--   -s,--src ARG             Source file to watch
--   -i,--interval INTERVAL   When running in a loop, check for changes every
--                            INTERVAL seconds.
-- @
--
--   For example, a common scenario is
--
-- @
-- $ ghc --make mydiagram
--
--   # output image.tex with a width of 400bp (and auto-determined height)
--   # (bp = big point = 1px at 72dpi)
-- $ ./mydiagram -o image.tex -w 400
-- @

defaultMain :: Diagram PGF -> IO ()
defaultMain = mainWith

-- | Allows you to pick a surface the diagram will be rendered with.
-- (This
mainWithSurf :: Surface -> Diagram PGF -> IO ()
mainWithSurf = curry mainWith

-- For online diagrams.

-- | Same as @defaultMain@ but takes an online pgf diagram.
onlineMain :: OnlineTex (Diagram PGF) -> IO ()
onlineMain = mainWith

-- | Same as @mainWithSurf@ but takes an online pgf diagram.
onlineMainWithSurf :: Surface -> OnlineTex (Diagram PGF) -> IO ()
onlineMainWithSurf = curry mainWith

-- Mainable instances

instance TypeableFloat n => Mainable (QDiagram PGF V2 n Any) where
  type MainOpts (QDiagram PGF V2 n Any) =
    (DiagramOpts, PGFCmdLineOpts, TexFormat)
  mainRender (diaOpts, pgfOpts, format) d = do
    chooseRender diaOpts pgfOpts (formatToSurf format) d

instance TypeableFloat n => Mainable (Surface, QDiagram PGF V2 n Any) where
  type MainOpts (Surface, QDiagram PGF V2 n Any) =
    (DiagramOpts, PGFCmdLineOpts)
  mainRender (diaOpts, pgfOpts) (surf,d) = do
    chooseRender diaOpts pgfOpts surf d

-- Online diagrams
instance TypeableFloat n => Mainable (OnlineTex (QDiagram PGF V2 n Any)) where
  type MainOpts (OnlineTex (QDiagram PGF V2 n Any))
    = (DiagramOpts, PGFCmdLineOpts, TexFormat)
  mainRender (diaOpts, pgfOpts, format) d = do
    chooseOnlineRender diaOpts pgfOpts (formatToSurf format) d

instance TypeableFloat n => Mainable (Surface, OnlineTex (QDiagram PGF V2 n Any)) where
  type MainOpts (Surface, OnlineTex (QDiagram PGF V2 n Any))
    = (DiagramOpts, PGFCmdLineOpts)
  mainRender (diaOpts, pgfOpts) (surf, d) = do
    chooseOnlineRender diaOpts pgfOpts surf d

formatToSurf :: TexFormat -> Surface
formatToSurf format = case format of
  LaTeX    -> latexSurface
  ConTeXt  -> contextSurface
  PlainTeX -> plaintexSurface

cmdLineOpts :: TypeableFloat n
   => DiagramOpts -> Surface -> PGFCmdLineOpts -> Options PGF V2 n
cmdLineOpts opts surf pgf
  = def & surface    .~ surf
        & sizeSpec   .~ sz
        & readable   .~ pgf^.cmdReadable
        & standalone .~ pgf^.cmdStandalone
  where
    sz = fromIntegral <$> mkSizeSpec2D (opts^.width) (opts^.height)

chooseRender :: TypeableFloat n
  => DiagramOpts -> PGFCmdLineOpts -> Surface -> QDiagram PGF V2 n Any -> IO ()
chooseRender diaOpts pgfOpts surf d =
  case diaOpts^.output of
    ""  -> hPutBuilder stdout $ renderDia PGF opts d
    out -> renderPGF' out opts d
  where
    opts = cmdLineOpts diaOpts surf pgfOpts

chooseOnlineRender :: TypeableFloat n
  => DiagramOpts -> PGFCmdLineOpts -> Surface -> OnlineTex (QDiagram PGF V2 n Any) -> IO ()
chooseOnlineRender diaOpts pgfOpts surf d =
    case diaOpts^.output of
      ""  -> surfOnlineTexIO surf d >>= hPutBuilder stdout . renderDia PGF opts
      out -> renderOnlinePGF' out opts d
  where
    opts = cmdLineOpts diaOpts surf pgfOpts


-- | @multiMain@ is like 'defaultMain', except instead of a single
--   diagram it takes a list of diagrams paired with names as input.
--   The generated executable then takes a @--selection@ option
--   specifying the name of the diagram that should be rendered.  The
--   list of available diagrams may also be printed by passing the
--   option @--list@.
--
--   Example usage:
--
-- @
-- $ ghc --make MultiTest
-- [1 of 1] Compiling Main             ( MultiTest.hs, MultiTest.o )
-- Linking MultiTest ...
-- $ ./MultiTest --list
-- Available diagrams:
--   foo bar
-- $ ./MultiTest --selection bar -o Bar.tex -w 200
-- @

multiMain :: [(String, Diagram PGF)] -> IO ()
multiMain = mainWith

instance TypeableFloat n => Mainable [(String,QDiagram PGF V2 n Any)] where
  type MainOpts [(String,QDiagram PGF V2 n Any)]
      = (MainOpts (QDiagram PGF V2 n Any), DiagramMultiOpts)

  mainRender = defaultMultiMainRender

instance Parseable TexFormat where
  parser = OP.option (eitherReader parseFormat)
                      $ short   'f'
                     <> long    "format"
                     <> help    "l for LaTeX, c for ConTeXt, p for plain TeX"
                     <> metavar "FORMAT"
                     <> OP.value LaTeX
                     <> showDefault

parseFormat :: String -> Either String TexFormat
parseFormat ('l':_) = Right LaTeX
parseFormat ('c':_) = Right ConTeXt
parseFormat ('p':_) = Right PlainTeX
parseFormat ('t':_) = Right PlainTeX
parseFormat x       = Left $ "Unknown format" ++ x

