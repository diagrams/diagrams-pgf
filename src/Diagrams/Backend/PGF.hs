{-# LANGUAGE TypeFamilies #-}

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
-- * You can use 'renderPGF' function to render a 'Diagram' to a ".tex" or
--   ".pdf" file;
--
-- * You can use the most flexible 'renderDia' function to access the
--   resulting PGF code directly in memory. For this particular backend
--   the 'renderDia' function has the following type:
--
-- > renderDia :: PGF -> Options PGF R2 -> Diagram PGF R2 -> Blaze.Builder
--
-- The 'Surface' provides the necessary information for rendering PGF code and
-- building a PDF using "texrunner".
--

module Diagrams.Backend.PGF
  ( -- * Rendering token & options
    PGF (..)
  , B
  -- , Options (..)
    -- * Rendering functions
  , renderPGF
  , renderPGF'
    -- ** Options lenses
  , readable
  , szSpec
  , surface
  , standalone
    -- * Online TeX
  , OnlineTeX
  , renderOnlinePGF
  , renderOnlinePGF'
    -- * TeX specific
  , hbox
  , onlineHbox
  , surfOnlineTex
    -- * Surfaces
    -- | These surfaces should be suitable for basic diagrams. For more
    --   complicated output options see 'Diagrams.Backend.PGF.Surface'.
  , Surface (..)
  , TeXFormat (..)
  , module Diagrams.Backend.PGF.Surface
  ) where

import           Control.Lens                 (set, (^.))
import           Data.ByteString.Builder
import           Data.Default
import           System.Directory             hiding (readable)
import           System.FilePath
import           System.IO
import           System.TeXRunner
import           System.TeXRunner.Online      hiding (hbox)
import           System.TeXRunner.Parse       (prettyPrintLog)

import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy.Char8   as LB

import           Diagrams.Backend.PGF.Hbox
import           Diagrams.Backend.PGF.Render
import           Diagrams.Backend.PGF.Surface
import           Diagrams.Size
import           Diagrams.Prelude             hiding (r2, view)


type B = PGF

type instance V PGF = V2
type instance N PGF = Double

-- | Render a pgf diagram and write it to the given filepath. If the file has
--   the extension @.pdf@, a PDF is generated in a temporary directory using
--   options from the given surface.
renderPGF :: TypeableFloat n
          => FilePath         -- ^ path to output
          -> SizeSpec V2 n    -- ^ size of output
          -> Surface          -- ^ 'Surface' to use
          -> QDiagram PGF V2 n Any -- ^ 'Diagram' to render
          -> IO ()
renderPGF outFile sizeSp surf = renderPGF' outFile opts
  where
    opts = case takeExtension outFile of
             ".pdf" -> def & surface    .~ surf
                           & szSpec     .~ sizeSp
                           & readable   .~ False
                           & standalone .~ True

             _      -> def & surface  .~ surf
                           & szSpec   .~ sizeSp

-- | Same as 'renderPGF' but takes 'Options PGF R2'.
renderPGF' :: TypeableFloat n => FilePath -> Options PGF V2 n -> QDiagram PGF V2 n Any -> IO ()
renderPGF' outFile opts d = case takeExtension outFile of
  ".pdf" -> do
    let rendered = renderDia PGF (opts & standalone .~ True) d

    currentDir <- getCurrentDirectory
    targetDir  <- canonicalizePath (takeDirectory outFile)

    let source = toLazyByteString rendered

    (_, texLog, mPDF) <- runTex (opts^.surface.command)
                                (opts^.surface.arguments)
                                [currentDir, targetDir]
                                source

    case mPDF of
      Nothing  -> putStrLn "Error, no PDF found:"
               >> B.putStrLn (prettyPrintLog texLog)
      Just pdf -> LB.writeFile outFile pdf

  -- tex output
  _      -> writeTexFile outFile opts d

-- | Render online PDF by calling TeX in a temporary directory.
renderOnlinePGF :: TypeableFloat n
                => FilePath
                -> SizeSpec V2 n
                -> Surface
                -> OnlineTeX (QDiagram PGF V2 n Any)
                -> IO ()
renderOnlinePGF outFile sizeSp surf = renderOnlinePGF' outFile opts
  where
    opts = def & szSpec  .~ sizeSp
               & surface .~ surf

-- | Same as 'renderOnlinePDF' but takes 'Options PGF R2'.
renderOnlinePGF' :: TypeableFloat n
                 => FilePath
                 -> Options PGF V2 n
                 -> OnlineTeX (QDiagram PGF V2 n Any)
                 -> IO ()
renderOnlinePGF' outFile opts dOL = case takeExtension outFile of
  ".pdf" -> do

    ((), texLog, mPDF) <-
      runOnlineTex'
        (surf^.command)
        (surf^.arguments)
        (B.pack $ surf^.preamble ++ surf^.beginDoc) $ do

          d <- dOL

          -- we've already output the preamble so don't do it again
          let opts' = opts & surface    %~ set beginDoc "" . set preamble ""
                           & readable   .~ False
                           & standalone .~ True

              rendered = renderDia PGF opts' d

          texPutStrLn $ (LB.toStrict . toLazyByteString) rendered

    case mPDF of
      Nothing  -> putStrLn "Error, no PDF found:"
               >> print texLog
      Just pdf -> LB.writeFile outFile pdf

  -- tex output
  _      ->  surfOnlineTexIO surf dOL >>= writeTexFile outFile opts
  where
    surf = opts ^. surface

-- | Write the rendered diagram to a text file, ignoring the extension.
writeTexFile :: TypeableFloat n => FilePath -> Options PGF V2 n -> QDiagram PGF V2 n Any -> IO ()
writeTexFile outFile opts d = do
  h <- openFile outFile WriteMode
  hPutBuilder h $ renderDia PGF opts d
  hClose h

