-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.PGF
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A full-featured PGF backend for diagrams producing PGF code
-- suitable for LaTeX, ConTeXt or plain TeX consumtion.
--
-- To invoke the PGF backend, you have a number of options.
--
-- * You can use 'renderPGF' function to render a 'Diagram' to a ".tex" file;
--
-- * You can use the most flexible 'renderDia' function to access the
-- resulting PGF code directly in memory. For this particular backend
-- the 'renderDia' function has the following type:
--
-- > renderDia :: PGF -> Options PGF R2 -> Diagram PGF R2 -> Blaze.Builder
--
-- The @Options PGF R2@ is specified as following:
--
-- > data Options PGF R2 = PGFOptions
-- >     { _template          :: Surface    -- ^ Surface you want to use.
-- >     , _sizeSpec          :: SizeSpec2D -- ^ The requested size.
-- >     , _readable          :: Bool       -- ^ Pretty print output.
-- >     }
--
-- The 'Surface' is simply a pair of a @header@ and a @footer@. You
-- can use a 'defaultSurface', as well as a number of other predefined
-- surfaces (which can be found in 'Diagrams.Backend.PGF.Surface').
--
-- It is also possible (although not necessary) to use 'Lens'es to
-- access fields of all the datastructures defined in this backend.
module Diagrams.Backend.PGF
    ( -- * Rendering token & options
      PGF (..)
    , B
    , Options (..)
      -- * TeX specific
    , hbox
      -- * Options lenses
    , surface, sizeSpec, readable
    , SizeSpec2D(..)
    , module Diagrams.Backend.PGF.Surface
    , defaultSurface
      -- * Rendering functions
    , renderDia
    , renderPGF
    , renderPGF'
    , renderPDF
    , renderProcessPDF
    , renderPDF'
    ) where

import Control.Lens ((^.), set)
import Data.Default
import Diagrams.Prelude     hiding (r2, view, (<.>))
import System.Directory     hiding (readable)
import System.Exit
import System.FilePath
import System.Process
import System.IO
import System.TeXRunner
import qualified System.TeXRunner.Online as Online

import qualified Blaze.ByteString.Builder      as Blaze
import qualified Data.ByteString.Char8         as B

import Diagrams.Backend.PGF.Hbox
import Diagrams.Backend.PGF.Render
import Diagrams.Backend.PGF.Surface


type B = PGF

-- |
-- @
--  defaultSurface = 'PGFSurface'
-- @
defaultSurface :: Surface
defaultSurface = def

-- | Render a diagram as a PGF code, writing to the specified output
--   file and using the requested size and surface, ready for inclusion in a
--   TeX document.
renderPGF :: FilePath -> SizeSpec2D -> Surface -> Diagram PGF R2 -> IO ()
renderPGF filePath sizeSp surf
  = renderPGF' filePath (def & surface .~ surf & sizeSpec .~ sizeSp)

-- | Similar to 'renderPDF' but takes PGFOptions instead.
renderPGF' :: FilePath -> Options PGF R2 -> Diagram PGF R2 -> IO ()
renderPGF' filePath ops dia = do
  h <- openFile filePath WriteMode
  let rendered = renderDia PGF ops dia
  Blaze.toByteStringIO (B.hPutStr h) rendered
  hClose h

-- | Render PDF by calling TeX in a temporary directory.
renderPDF :: FilePath -> SizeSpec2D -> Surface -> Diagram PGF R2 -> IO ()
renderPDF filePath sizeSp surf dia = do

  let rendered = renderDia PGF (def & surface    .~ surf
                                    & sizeSpec   .~ sizeSp
                                    & readable   .~ True
                                    & standalone .~ True)
                               dia

  flip Blaze.toByteStringIO rendered $ \source -> do
    (_, texLog, mPDF) <- runTex (surf^.command) (surf^.arguments) source

    case mPDF of
      Nothing  -> putStrLn "Error, no PDF found:"
               >> print texLog
      Just pdf -> B.writeFile filePath pdf


-- | Render PDF by calling TeX in a temporary directory.
renderProcessPDF :: FilePath
                 -> SizeSpec2D
                 -> Surface
                 -> Online.TeXProcess (Diagram PGF R2)
                 -> IO ()
renderProcessPDF filePath sizeSp surf diaP = do

  ((), texLog, mPDF) <-
    Online.runTexOnline
      (surf^.command)
      (surf^.arguments)
      (B.pack $ surf^.preamble) $ do

        dia <- diaP

        let rendered = renderDia PGF (def & surface    .~ set preamble "" surf
                                          & sizeSpec   .~ sizeSp
                                          & readable   .~ True
                                          & standalone .~ True
                                     ) dia

        Online.texPutStrLn $ Blaze.toByteString rendered

  case mPDF of
    Nothing  -> putStrLn "Error, no PDF found:"
             >> print texLog
    Just pdf -> B.writeFile filePath pdf


-- | Render PGF and save to output.tex and run surface command on output.tex.
--   All auxillery files are kept.
renderPDF' :: FilePath -> SizeSpec2D -> Surface -> Diagram PGF R2 -> IO ()
renderPDF' filePath sizeSp surf dia = do
  let (outDir,fileName) = splitFileName filePath
      texFileName       = replaceExtension fileName "tex"
  oldDir <- getCurrentDirectory
  setCurrentDirectory outDir
  --
  renderPGF' texFileName (def & sizeSpec   .~ sizeSp
                              & surface    .~ surf
                              & standalone .~ True)
                         dia
  (ecode, _, _)
    <- readProcessWithExitCode (surf^.command) (texFileName : surf^.arguments) ""

  setCurrentDirectory oldDir

  case ecode of
    ExitFailure _ -> do
      let logFile = replaceExtension filePath "log"
      logExists <- doesFileExist logFile
      putStrLn $ "An error occured while processing TeX file"
              ++ if logExists
                   then "please check " ++ logFile
                   else "and no log file was found"
    ExitSuccess   -> return ()

