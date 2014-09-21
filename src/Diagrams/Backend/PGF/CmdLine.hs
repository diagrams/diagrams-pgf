{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import           Diagrams.Prelude          hiding (height, interval, width, (<>))

import           Control.Lens
import           Control.Monad             (mplus)
import           Data.Default

import           Data.ByteString.Builder
import           Options.Applicative       as OP

#ifdef CMDLINELOOP
import           Control.Concurrent        (threadDelay)
import           Control.Exception         (SomeException (..))
import qualified Control.Exception         as Exc (bracket, catch)
import           Control.Monad             (when)
import           Data.Maybe                (fromMaybe)
import           System.Directory          (getModificationTime)
import           System.Exit               (ExitCode (..))
import           System.IO                 (BufferMode (..), IOMode (..), hClose, hSetBuffering,
                                            openFile, stdout)
import           System.Process            (runProcess, waitForProcess)

import           System.Environment        (getArgs, getProgName)
import           System.Posix.Process      (executeFile)

# if MIN_VERSION_directory(1,2,0)
import           Data.Time.Clock           (UTCTime, getCurrentTime)
type ModuleTime = UTCTime
getModuleTime :: IO ModuleTime
getModuleTime = getCurrentTime
#else
import           System.Time               (ClockTime, getClockTime)
type ModuleTime = ClockTime
getModuleTime :: IO ModuleTime
getModuleTime = getClockTime
#endif
#endif

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
-- > f :: Int -> Colour Double -> Diagram PGF V2 n
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

defaultMain :: DataFloat n => Diagram PGF V2 n -> IO ()
defaultMain = mainWith

instance DataFloat n => Mainable (Diagram PGF V2 n) where
#ifdef CMDLINELOOP
  type MainOpts (Diagram PGF V2 n)
    = (DiagramOpts, (TeXFormat, (PGFCmdLineOpts, DiagramLoopOpts)))

  mainRender (diaOpts,(format,(pgfOpts,loopOpts))) d = do
      let opts = cmdLineOpts diaOpts (formatToSurf format) pgfOpts
      case diaOpts^.output of
        ""  -> hPutBuilder stdout $ renderDia PGF opts d
        out -> renderPGF' out opts d
      when (loopOpts^.loop) (waitForChange Nothing loopOpts)
#else
  type MainOpts (Diagram PGF V2 n) = (DiagramOpts, (TeXFormat, PGFCmdLineOpts))

  mainRender (diaOpts, (format, pgfOpts))
    = let opts = cmdLineOpts diaOpts (formatToSurf format) pgfOpts
      in  case diaOpts^.output of
            ""  -> hPutBuilder stdout $ renderDia PGF pgfOpts d
            out -> renderPGF' out opts d
#endif

cmdLineOpts :: DataFloat n
   => DiagramOpts -> Surface -> PGFCmdLineOpts -> Options PGF V2 n
cmdLineOpts opts surf pgf
  = def & surface    .~ surf
        & sizeSpec   .~ sz
        & readable   .~ pgf^.cmdReadable
        & standalone .~ pgf^.cmdStandalone
  where
    sz = mkSizeSpec (f $ opts^.width) (f $ opts^.height)
    f  = fmap fromIntegral

chooseRender :: DataFloat n
             => DiagramOpts
             -> Surface
             -> PGFCmdLineOpts
             -> Diagram PGF V2 n
             -> IO ()
chooseRender opts surf pgf d = case opts^.output of
    ""  -> hPutBuilder stdout $ renderDia PGF pgfOpts d
    out -> renderPGF' out pgfOpts d
  where
    pgfOpts = def & surface    .~ surf
                  & sizeSpec   .~ sz
                  & readable   .~ pgf^.cmdReadable
                  & standalone .~ pgf^.cmdStandalone

    sz = case (opts^.width, opts^.height) of
           (Nothing, Nothing) -> Absolute
           (Just w, Nothing)  -> Width (fromIntegral w)
           (Nothing, Just h)  -> Height (fromIntegral h)
           (Just w, Just h)   -> Dims (fromIntegral w) (fromIntegral h)

formatToSurf :: TeXFormat -> Surface
formatToSurf format = case format of
  LaTeX    -> latexSurface
  ConTeXt  -> contextSurface
  PlainTeX -> plaintexSurface

-- | Allows you to pick a surface the diagram will be rendered with.
mainWithSurf :: DataFloat n => Surface -> Diagram PGF V2 n -> IO ()
mainWithSurf = curry mainWith


instance DataFloat n => Mainable (Surface, Diagram PGF V2 n) where
  type MainOpts (Surface, Diagram PGF V2 n) = (DiagramOpts, PGFCmdLineOpts)

  mainRender (opts,pgf) (surf,d) = chooseRender opts surf pgf d

-- online pgf diagrams

onlineChooseRender :: DataFloat n
                   => DiagramOpts
                   -> Surface
                   -> PGFCmdLineOpts
                   -> OnlineTeX (Diagram PGF V2 n)
                   -> IO ()
onlineChooseRender opts surf pgf dOL = case opts^.output of
    ""  -> do d <- surfOnlineTexIO surf dOL
              hPutBuilder stdout $ renderDia PGF pgfOpts d

    out -> renderOnlinePGF' out pgfOpts dOL
  where
    pgfOpts = def & surface    .~ surf
                  & sizeSpec   .~ sz
                  & readable   .~ (pgf^.cmdReadable)
                  & standalone .~ (pgf^.cmdStandalone)

    sz = case (opts^.width, opts^.height) of
           (Nothing, Nothing) -> Absolute
           (Just w, Nothing)  -> Width (fromIntegral w)
           (Nothing, Just h)  -> Height (fromIntegral h)
           (Just w, Just h)   -> Dims (fromIntegral w) (fromIntegral h)

instance DataFloat n => Mainable (OnlineTeX (Diagram PGF V2 n)) where
  type MainOpts (OnlineTeX (Diagram PGF V2 n))
    = (DiagramOpts, (PGFCmdLineOpts, TeXFormat))

  mainRender (diaOpts,(pgfOpts,format)) dOL
    -- = onlineChooseRender opts (formatToSurf format) pgf
    = let surf = formatToSurf format
          opts = cmdLineOpts diaOpts surf pgfOpts
      in  case diaOpts^.output of
            -- ""  -> hPutBuilder stdout $ renderDia PGF opts d
            ""  -> surfOnlineTexIO surf dOL >>= hPutBuilder stdout . renderDia PGF opts
            out -> renderOnlinePGF' out opts dOL

instance DataFloat n => Mainable (Surface, OnlineTeX (Diagram PGF V2 n)) where
  type MainOpts (Surface, OnlineTeX (Diagram PGF V2 n))
    = (DiagramOpts, PGFCmdLineOpts)

  mainRender (opts,pgf) (surf,d) = onlineChooseRender opts surf pgf d

-- | Same as @defaultMain@ but takes an online pgf diagram.
onlineMain :: DataFloat n => OnlineTeX (Diagram PGF V2 n) -> IO ()
onlineMain = mainWith

-- | Same as @mainWithSurf@ but takes an online pgf diagram.
onlineMainWithSurf :: DataFloat n => Surface -> OnlineTeX (Diagram PGF V2 n) -> IO ()
onlineMainWithSurf = curry mainWith

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

multiMain :: DataFloat n => [(String, Diagram PGF V2 n)] -> IO ()
multiMain = mainWith

instance DataFloat n => Mainable [(String,Diagram PGF V2 n)] where
    type MainOpts [(String,Diagram PGF V2 n)]
        = (MainOpts (Diagram PGF V2 n), DiagramMultiOpts)

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
            else go prog args $ newAttempt `mplus` lastAttempt

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

