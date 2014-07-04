-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.PGF
-- Copyright   :  (c) 2014 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  c.chalmers@me.com
--
-- A full-featured PGF backend for diagrams producing PGF code
-- suitable for LaTeX, ConTeXt or plain TeX consumption.
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
-- The 'Surface' provides the necessary information to build a PDF using 
-- "texrunner"
--

module Diagrams.Backend.PGF
  ( -- * Rendering token & options
    PGF (..)
  , B
  -- , Options (..)
    -- * Rendering functions
  , renderPGF
  , renderPGF'
  , renderOnlinePDF
    -- * Options lenses
  , readable
  , sizeSpec
  , surface
  , standalone
    -- * TeX specific
  , hbox
  , onlineHbox
  , surfOnlineTex
    -- * Surfaces
    -- | These surfaces should be suitable for basic diagrams. For more 
    --   complicated diagrams see 'Diagrams.Backend.PGF.Surface'.
  , TeXFormat (..)
  , latexSurface
  , contextSurface
  , plaintexSurface
  ) where

import Control.Lens ((^.), set)
import Data.Default
import Diagrams.Prelude     hiding (r2, view, (<.>))
import System.Directory hiding (readable)
import System.FilePath
import System.IO
import System.TeXRunner
import System.TeXRunner.Online hiding (hbox)

import qualified Blaze.ByteString.Builder   as Blaze
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B

import Diagrams.Backend.PGF.Hbox
import Diagrams.Backend.PGF.Render
import Diagrams.Backend.PGF.Surface


type B = PGF

renderPGF :: FilePath -> SizeSpec2D -> Surface -> Diagram PGF R2 -> IO ()
renderPGF outFile sizeSp surf = renderPGF' outFile ops 
  where
    ops = case takeExtension outFile of 
            ".pdf" -> def & surface    .~ surf
                          & sizeSpec   .~ sizeSp
                          & readable   .~ False
                          & standalone .~ True
            _      -> def & surface    .~ surf
                          & sizeSpec   .~ sizeSp

renderPGF' :: FilePath -> Options PGF R2 -> Diagram PGF R2 -> IO ()
renderPGF' outFile ops dia = case takeExtension outFile of
  ".pdf" -> do
    let rendered = renderDia PGF (ops & standalone .~ True) dia

    currentDir <- getCurrentDirectory
    targetDir  <- canonicalizePath (takeDirectory outFile)

    let source = Blaze.toLazyByteString rendered

    (_, texLog, mPDF) <- runTex (ops^.surface.command) (ops^.surface.arguments) [currentDir, targetDir] source

    case mPDF of
      Nothing  -> putStrLn "Error, no PDF found:"
               >> print texLog
      Just pdf -> LB.writeFile outFile pdf

  -- tex output
  _      -> do
    h <- openFile outFile WriteMode
    let rendered = renderDia PGF ops dia
    Blaze.toByteStringIO (B.hPutStr h) rendered
    hClose h

-- -- | render a diagram as a pgf code, writing to the specified output
-- --   file and using the requested size and surface, ready for inclusion in a
-- --   TeX document.
-- renderPGF :: FilePath -> SizeSpec2D -> Surface -> Diagram PGF R2 -> IO ()
-- renderPGF filePath sizeSp surf
--   = renderPGF' filePath (def & surface .~ surf & sizeSpec .~ sizeSp)
-- 
-- -- | Similar to 'renderPDF' but takes PGFOptions instead.
-- renderPGF' :: FilePath -> Options PGF R2 -> Diagram PGF R2 -> IO ()
-- renderPGF' filePath ops dia = do
--   h <- openFile filePath WriteMode
--   let rendered = renderDia PGF ops dia
--   Blaze.toByteStringIO (B.hPutStr h) rendered
--   hClose h
-- 
-- -- | Render PDF by calling TeX in a temporary directory.
-- renderPDF :: FilePath -> SizeSpec2D -> Surface -> Diagram PGF R2 -> IO ()
-- renderPDF filePath sizeSp surf dia = do
-- 
--   let rendered = renderDia PGF (def & surface    .~ surf
--                                     & sizeSpec   .~ sizeSp
--                                     & readable   .~ False
--                                     & standalone .~ True)
--                                dia
-- 
--   flip Blaze.toByteStringIO rendered $ \source -> do
--     (_, texLog, mPDF) <- runTex (surf^.command) (surf^.arguments) source
-- 
--     case mPDF of
--       Nothing  -> putStrLn "Error, no PDF found:"
--                >> print texLog
--       Just pdf -> B.writeFile filePath pdf


-- | Render PDF by calling TeX in a temporary directory.
renderOnlinePDF :: FilePath
                -> SizeSpec2D
                -> Surface
                -> OnlineTeX (Diagram PGF R2)
                -> IO ()
renderOnlinePDF filePath sizeSp surf diaP = do

  ((), texLog, mPDF) <-
    runOnlineTex'
      (surf^.command)
      (surf^.arguments)
      (B.pack $ surf^.preamble) $ do

        dia <- diaP

        -- we've already output the preamble so don't do it again
        let rendered = renderDia PGF (def & surface    .~ set preamble "" surf
                                          & sizeSpec   .~ sizeSp
                                          & readable   .~ False
                                          & standalone .~ True
                                     ) dia

        texPutStrLn $ Blaze.toByteString rendered

  case mPDF of
    Nothing  -> putStrLn "Error, no PDF found:"
             >> print texLog
    Just pdf -> LB.writeFile filePath pdf


-- -- | Render PGF and save to output.tex and run surface command on output.tex.
-- --   All auxillery files are kept.
-- renderPDF' :: FilePath -> SizeSpec2D -> Surface -> Diagram PGF R2 -> IO ()
-- renderPDF' filePath sizeSp surf dia = do
--   let (outDir,fileName) = splitFileName filePath
--       texFileName       = replaceExtension fileName "tex"
--   oldDir <- getCurrentDirectory
--   setCurrentDirectory outDir
--   --
--   renderPGF' texFileName (def & sizeSpec   .~ sizeSp
--                               & surface    .~ surf
--                               & standalone .~ True)
--                          dia
--   (ecode, _, _)
--     <- readProcessWithExitCode (surf^.command) (texFileName : surf^.arguments) ""
-- 
--   setCurrentDirectory oldDir
-- 
--   case ecode of
--     ExitFailure _ -> do
--       let logFile = replaceExtension filePath "log"
--       logExists <- doesFileExist logFile
--       putStrLn $ "An error occured while processing TeX file"
--               ++ if logExists
--                    then "please check " ++ logFile
--                    else "and no log file was found"
--     ExitSuccess   -> return ()
-- 
