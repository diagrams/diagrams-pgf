{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.PGF
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A full-featured PGFSystem backend for diagrams producing PGFSystem code
-- suitable for LaTeX, ConTeXt or plain TeX consumtion.
--
-- To invoke the PGFSystem backend, you have a number of options.
--
-- * You can use 'renderPGF' function to render a 'Diagram' to a ".tex" file;
--
-- * You can use the most flexible 'renderDia' function to access the
-- resulting PGFSystem code directly in memory. For this particular backend
-- the 'renderDia' function has the following type:
--
-- > renderDia :: PGFSystem -> Options PGFSystem R2 -> Diagram PGFSystem R2 -> Blaze.Builder
--
-- The @Options PGFSystem R2@ is specified as following:
--
-- > data Options PGFSystem R2 = PGFOptions
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
module Diagrams.Backend.PGFSystem
    ( -- * Rendering token & options
      PGFSystem (..)
    , B
    , Options (..)
      -- ** Options lenses
    , surface, sizeSpec --, readable
    , SizeSpec2D(..)
    , module Diagrams.Backend.PGFSystem.Surface
    -- , defaultSurface
      -- * Rendering functions
    , renderDia
    , renderPGF
    , renderPGF'
    , renderPDF
    , renderPDF'
    -- , sizeSpecToBounds
    ) where

import Control.Lens ((^.))
import Control.Monad (when)
import Data.Default
import Diagrams.Prelude     hiding (r2, view, (<.>))
import System.Directory     hiding (readable)
import System.Exit
import System.FilePath
import System.Process
import System.IO

import qualified Blaze.ByteString.Builder      as Blaze
import qualified Data.ByteString.Char8         as B

import Diagrams.Backend.PGFSystem.Render
import Diagrams.Backend.PGFSystem.Surface


type B = PGFSystem

-- -- |
-- -- @
-- --  defaultSurface = 'PGFSurface'
-- -- @
-- defaultSurface :: Surface
-- defaultSurface = def

-- | Render a diagram as a PGFSystem code, writing to the specified output
--   file and using the requested size and surface, ready for inclusion in a 
--   TeX document.
renderPGF :: FilePath -> SizeSpec2D -> Surface -> Diagram PGFSystem R2 -> IO ()
renderPGF filePath sizeSp surf
  = renderPGF' filePath (def & surface .~ surf & sizeSpec .~ sizeSp)

-- | Similar to 'renderPDF' but takes PGFOptions instead.
renderPGF' :: FilePath -> Options PGFSystem R2 -> Diagram PGFSystem R2 -> IO ()
renderPGF' filePath ops dia = do
    h <- openFile filePath WriteMode
    let rendered = renderDia PGFSystem ops dia
    Blaze.toByteStringIO (B.hPutStr h) rendered
    hClose h

-- | This is an experimental function that pipes directly to pdfTeX. It's a 
--   little hacky and might not always work. It should be faster as pdfTeX can 
--   load as diagrams output is generated. If you want to save the .tex file 
--   aswell, use @renderPDF'@.
renderPDF :: FilePath -> SizeSpec2D -> Surface -> Diagram PGFSystem R2 -> IO ()
renderPDF filePath sizeSp surf dia = do
  tmp <- getTemporaryDirectory
  let jobName = "texput" -- TODO make a random one
      args = (surf^.jobArg) jobName : surf^.arguments
      p = (proc (surf^.command) args)
           { cwd     = Just tmp
           , std_in  = CreatePipe
           , std_out = CreatePipe
           }

  (Just inH, Just outH, _, pHandle) <- createProcess p

  -- this is important, TeX doesn't work unless you gobble it's output
  _ <- hGetContents outH

  -- this is important, each \n must be followed by a flush or TeX breaks
  hSetBuffering inH LineBuffering

  let rendered = renderDia PGFSystem (def & surface    .~ surf
                                          & sizeSpec   .~ sizeSp)
                               dia
  Blaze.toByteStringIO (B.hPutStr inH) rendered
  hClose inH
  exitC <- waitForProcess pHandle
  -- must't be closed before process has finished
  hClose outH

  let logFile = tmp </> jobName <.> "log"
  logExists <- doesFileExist logFile

  case exitC of
    ExitSuccess -> do
      copyFile (tmp </> jobName <.> "pdf") filePath
      when (surf^.texFormat == ConTeXt) $ pdfcrop filePath
      when logExists $ removeFile logFile
      removeFile (tmp </> jobName <.> "pdf")
    ExitFailure _ -> do
      putStrLn $ "An error occured while processing TeX file"
              ++ if logExists
                   then "please check " ++ logFile
                   else "and no log file was found"
      when logExists $ copyFile logFile (replaceExtension filePath "log")

-- | Tempory solution to crop ConTeXt document, makes generation slow.
pdfcrop :: FilePath -> IO ()
pdfcrop path = do
  (eCode, _,err) <- readProcessWithExitCode "pdfcrop" [path, path] ""

  case eCode of
    ExitFailure _ -> putStrLn $ "pdfcrop failed: " ++ err
    _             -> return ()


-- | Render PGFSystem and save to output.tex and run surface command on output.tex. 
--   All auxillery files are kept.
renderPDF' :: FilePath -> SizeSpec2D -> Surface -> Diagram PGFSystem R2 -> IO ()
renderPDF' filePath sizeSp surf dia = do
    let (outDir,fileName) = splitFileName filePath
        texFileName       = replaceExtension fileName "tex"
    oldDir <- getCurrentDirectory
    setCurrentDirectory outDir
    --
    renderPGF' texFileName (def & sizeSpec .~ sizeSp
                                & surface .~ surf
                                )
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

