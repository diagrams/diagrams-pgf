{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.PGF.CmdLine
-- Copyright   :  (c) 2014-2015 Diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  c.chalmers@me.com
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

import           Diagrams.Backend.CmdLine
import           Diagrams.Backend.PGF
import           Diagrams.Backend.PGF.Hbox
import           Diagrams.Prelude          hiding (height, interval, output,
                                            width, (<>))

import           Data.ByteString.Builder
import           Options.Applicative       as OP

import           System.IO                 (stdout)

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
instance ToResult d => ToResult (OnlineTeX d) where
  type Args (OnlineTeX d) = (Surface, Args d)
  type ResultOf (OnlineTeX d) = IO (ResultOf d)

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
-- > f :: Int -> Colour Double -> QDiagram PGF V2 n Any
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

defaultMain :: TypeableFloat n => QDiagram PGF V2 n Any -> IO ()
defaultMain = mainWith

-- | Allows you to pick a surface the diagram will be rendered with.
mainWithSurf :: TypeableFloat n => Surface -> QDiagram PGF V2 n Any -> IO ()
mainWithSurf = curry mainWith

-- For online diagrams.

-- | Same as @defaultMain@ but takes an online pgf diagram.
onlineMain :: TypeableFloat n => OnlineTeX (QDiagram PGF V2 n Any) -> IO ()
onlineMain = mainWith

-- | Same as @mainWithSurf@ but takes an online pgf diagram.
onlineMainWithSurf :: TypeableFloat n => Surface -> OnlineTeX (QDiagram PGF V2 n Any) -> IO ()
onlineMainWithSurf = curry mainWith

-- Mainable instances

instance TypeableFloat n => Mainable (QDiagram PGF V2 n Any) where
  type MainOpts (QDiagram PGF V2 n Any) =
    (DiagramOpts, DiagramLoopOpts, PGFCmdLineOpts, TeXFormat)
  mainRender (diaOpts, loopOpts, pgfOpts, format) d = do
    chooseRender diaOpts pgfOpts (formatToSurf format) d
    defaultLoopRender loopOpts

instance TypeableFloat n => Mainable (Surface, QDiagram PGF V2 n Any) where
  type MainOpts (Surface, QDiagram PGF V2 n Any) =
    (DiagramOpts, DiagramLoopOpts, PGFCmdLineOpts)
  mainRender (diaOpts, loopOpts, pgfOpts) (surf,d) = do
    chooseRender diaOpts pgfOpts surf d
    defaultLoopRender loopOpts

-- Online diagrams
instance TypeableFloat n => Mainable (OnlineTeX (QDiagram PGF V2 n Any)) where
  type MainOpts (OnlineTeX (QDiagram PGF V2 n Any))
    = (DiagramOpts, DiagramLoopOpts, PGFCmdLineOpts, TeXFormat)
  mainRender (diaOpts, loopOpts, pgfOpts, format) d = do
    chooseOnlineRender diaOpts pgfOpts (formatToSurf format) d
    defaultLoopRender loopOpts

instance TypeableFloat n => Mainable (Surface, OnlineTeX (QDiagram PGF V2 n Any)) where
  type MainOpts (Surface, OnlineTeX (QDiagram PGF V2 n Any))
    = (DiagramOpts, DiagramLoopOpts, PGFCmdLineOpts)
  mainRender (diaOpts, loopOpts, pgfOpts) (surf, d) = do
    chooseOnlineRender diaOpts pgfOpts surf d
    defaultLoopRender loopOpts

formatToSurf :: TeXFormat -> Surface
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
  => DiagramOpts -> PGFCmdLineOpts -> Surface -> OnlineTeX (QDiagram PGF V2 n Any) -> IO ()
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
-- $ ./MultiTest --selection bar -o Bar.eps -w 200
-- @

multiMain :: TypeableFloat n => [(String, QDiagram PGF V2 n Any)] -> IO ()
multiMain = mainWith

instance TypeableFloat n => Mainable [(String,QDiagram PGF V2 n Any)] where
    type MainOpts [(String,QDiagram PGF V2 n Any)]
        = (MainOpts (QDiagram PGF V2 n Any), DiagramMultiOpts)

    mainRender = defaultMultiMainRender

instance Parseable TeXFormat where
  parser = OP.option (eitherReader parseFormat)
                      $ short   'f'
                     <> long    "format"
                     <> help    "l for LaTeX, c for ConTeXt, p for plain TeX"
                     <> metavar "FORMAT"
                     <> OP.value LaTeX
                     <> showDefault

parseFormat :: String -> Either String TeXFormat
parseFormat ('l':_) = Right LaTeX
parseFormat ('c':_) = Right ConTeXt
parseFormat ('p':_) = Right PlainTeX
parseFormat ('t':_) = Right PlainTeX
parseFormat x       = Left $ "Unknown format" ++ x

