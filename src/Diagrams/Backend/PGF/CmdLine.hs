{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.PGF.CmdLine
-- Copyright   :  (c) 2014 Diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  c.chalmers@me.com
--
-- Convenient creation of command-line-driven executables for
-- rendering diagrams using the PGF backend.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.PGF.CmdLine
       ( 
         -- * General form of @main@
         -- $mainwith

         mainWith

         -- * Supported forms of @main@

       , defaultMain
       , multiMain

         -- * Backend tokens

       , PGF
       , B
       ) where

import Diagrams.Prelude hiding (width, height, interval)
import Diagrams.Backend.PGF
import Diagrams.Backend.CmdLine

import Control.Lens
import Data.Default

import Data.List.Split
import qualified Options.Applicative as OP
import qualified Data.ByteString as B
import qualified Blaze.ByteString.Builder as Blaze

#ifdef CMDLINELOOP
import           Data.Maybe          (fromMaybe)
import           Control.Monad       (when)
import           System.Directory    (getModificationTime)
import           System.Process      (runProcess, waitForProcess)
import           System.IO           (openFile, hClose, IOMode(..),
                                      hSetBuffering, BufferMode(..), stdout)
import           System.Exit         (ExitCode(..))
import                               Control.Concurrent  (threadDelay)
import qualified Control.Exception as Exc  (catch,  bracket)
import Control.Exception (SomeException(..))


import System.Environment  (getProgName,getArgs)
import System.Posix.Process (executeFile)


# if MIN_VERSION_directory(1,2,0)
import Data.Time.Clock (UTCTime,getCurrentTime)
type ModuleTime = UTCTime
getModuleTime :: IO  ModuleTime
getModuleTime = getCurrentTime
#else
import System.Time         (ClockTime, getClockTime)
type ModuleTime = ClockTime
getModuleTime :: IO  ModuleTime
getModuleTime = getClockTime
#endif
#endif

-- $mainwith
-- The 'mainWith' method unifies all of the other forms of @main@ and is
-- now the recommended way to build a command-line diagrams program.  It
-- works as a direct replacement for 'defaultMain' or 'multiMain' as well
-- as allowing more general arguments.  For example, given a function that
-- produces a diagram when given an @Int@ and a @'Colour' Double@, 'mainWith'
-- will produce a program that looks for additional number and color arguments.
--
-- > ... definitions ...
-- > f :: Int -> Colour Double -> Diagram PGF R2
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
-- ./mydiagram
--
-- Usage: ./mydiagram [-w|--width WIDTH] [-h|--height HEIGHT] [-o|--output OUTPUT] [-f|--format FORMAT] [-l|--loop] [-s|--src ARG] [-i|--interval INTERVAL]
--   Command-line diagram generation.
-- 
-- Available options:
--   -?,--help                Show this help text
--   -w,--width WIDTH         Desired WIDTH of the output image
--   -h,--height HEIGHT       Desired HEIGHT of the output image
--   -o,--output OUTPUT       OUTPUT file
--   -f,--format FORMAT       l for LaTeX, c for ConTeXt, p for plain TeX (default: LaTeX)
--   -l,--loop                Run in a self-recompiling loop
--   -s,--src ARG             Source file to watch
--   -i,--interval INTERVAL   When running in a loop, check for changes every INTERVAL seconds.
-- @
--
--   For example, a common scenario is
--
-- @
-- $ ghc --make MyDiagram
--
--   # output image.tex with a width of 400pt (and auto-determined height)
-- $ ./mydiagram -o image.tex -w 400
-- @

defaultMain :: Diagram PGF R2 -> IO ()
defaultMain = mainWith

instance Mainable (Diagram PGF R2) where
#ifdef CMDLINELOOP
    type MainOpts (Diagram PGF R2) = (DiagramOpts, (TeXFormat, DiagramLoopOpts))

    mainRender (opts,(format,loopOpts)) d = do
        chooseRender opts format d
        when (loopOpts^.loop) (waitForChange Nothing loopOpts)
#else
    type MainOpts (Diagram PGF R2) = (DiagramOpts, TeXFormat)

    mainRender (dOps, format) = chooseRender dOps format
#endif

chooseRender :: DiagramOpts -> TeXFormat -> Diagram PGF R2 -> IO ()
chooseRender opts format d = case splitOn "." (opts^.output) of
    [""] -> do
         let surf = case format of
                         LaTeX    -> latexSurface
                         ConTeXt  -> contextSurface
                         PlainTeX -> plaintexSurface
         Blaze.toByteStringIO B.putStr $ 
           renderDia PGF (def & template .~ surf & sizeSpec .~ size) d
    ps | last ps == "tex" -> do
         let surf = case format of
                         LaTeX    -> latexSurface
                         ConTeXt  -> contextSurface
                         PlainTeX -> plaintexStandaloneSurface
         renderPGF (opts^.output) size surf d
       --
       | last ps == "pdf" -> do
         let surf = case format of
                         LaTeX    -> latexStandaloneSurface 
                         ConTeXt  -> contextStandaloneSurface
                         PlainTeX -> plaintexStandaloneSurface
         renderPDF (opts^.output) size surf d
       --
       | otherwise -> putStrLn $ "Unknown file type: " ++ last ps
                              ++ "\nSupported file types are .tex or .pdf"
  where
    size = case (opts^.width, opts^.height) of
             (Nothing, Nothing) -> Absolute
             (Just w, Nothing)  -> Width (fromIntegral w)
             (Nothing, Just h)  -> Height (fromIntegral h)
             (Just w, Just h)   -> Dims (fromIntegral w) (fromIntegral h)


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

multiMain :: [(String, Diagram PGF R2)] -> IO ()
multiMain = mainWith

instance Mainable [(String,Diagram PGF R2)] where
    type MainOpts [(String,Diagram PGF R2)] 
        = (MainOpts (Diagram PGF R2), DiagramMultiOpts)

    mainRender = defaultMultiMainRender

instance Parseable TeXFormat where
  parser = OP.nullOption $ OP.eitherReader parseFormat
                     OP.<> OP.short   'f'
                     OP.<> OP.long    "format"
                     OP.<> OP.help    "l for LaTeX, c for ConTeXt, p for plain TeX"
                     OP.<> OP.metavar "FORMAT"
                     OP.<> OP.value LaTeX
                     OP.<> OP.showDefault

parseFormat :: String -> Either String TeXFormat
parseFormat ('l':_) = Right LaTeX
parseFormat ('c':_) = Right ConTeXt
parseFormat ('p':_) = Right PlainTeX
parseFormat ('t':_) = Right PlainTeX
parseFormat x       = Left $ "Unknow format" ++ x

#ifdef CMDLINELOOP
waitForChange :: Maybe ModuleTime -> DiagramLoopOpts -> IO ()
waitForChange lastAttempt opts = do
    prog <- getProgName
    args <- getArgs
    hSetBuffering stdout NoBuffering
    go prog args lastAttempt
  where go prog args lastAtt = do
          threadDelay (1000000 * opts^.interval)
          -- putStrLn $ "Checking... (last attempt = " ++ show lastAttempt ++ ")"
          (newBin, newAttempt) <- recompile lastAtt prog (opts^.src)
          if newBin
            then executeFile prog False args Nothing
            else go prog args $ getFirst (First newAttempt <> First lastAtt)

-- | @recompile t prog@ attempts to recompile @prog@, assuming the
--   last attempt was made at time @t@.  If @t@ is @Nothing@ assume
--   the last attempt time is the same as the modification time of the
--   binary.  If the source file modification time is later than the
--   last attempt time, then attempt to recompile, and return the time
--   of this attempt.  Otherwise (if nothing has changed since the
--   last attempt), return @Nothing@.  Also return a Bool saying
--   whether a successful recompilation happened.
recompile :: Maybe ModuleTime -> String -> Maybe String -> IO (Bool, Maybe ModuleTime)
recompile lastAttempt prog mSrc = do
  let errFile = prog ++ ".errors"
      srcFile = fromMaybe (prog ++ ".hs") mSrc
  binT <- maybe (getModTime prog) (return . Just) lastAttempt
  srcT <- getModTime srcFile
  if (srcT > binT)
    then do
      putStr "Recompiling..."
      status <- Exc.bracket (openFile errFile WriteMode) hClose $ \h ->
        waitForProcess =<< runProcess "ghc" ["--make", srcFile]
                           Nothing Nothing Nothing Nothing (Just h)

      if (status /= ExitSuccess)
        then putStrLn "" >> putStrLn (replicate 75 '-') >> readFile errFile >>= putStr
        else putStrLn "done."

      curTime <- getModuleTime
      return (status == ExitSuccess, Just curTime)

    else return (False, Nothing)

 where getModTime f = Exc.catch (Just <$> getModificationTime f)
                            (\(SomeException _) -> return Nothing)
#endif

