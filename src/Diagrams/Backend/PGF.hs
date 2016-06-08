{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.PGF
-- Copyright   :  (c) 2015 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A full-featured PGF backend for diagrams producing PGF code
-- suitable for LaTeX, ConTeXt or plain TeX consumption.
--
-- To invoke the PGF backend, you have a number of options.
--
-- * You can use 'renderPGF' or 'renderPGF'' to render a 'Diagram' to a
--   ".tex" or ".pdf" file;
--
-- * You can use the most flexible 'renderDia' function to access the
--   resulting PGF code directly in memory as a bytestring 'Builder'.
--
-- * Use 'Diagrams.Backend.PGF.CmdLine.mainWith' or
--   'Diagrams.Backend.PGF.CmdLine.defaultMain' to make a command line
--   application. See 'Diagrams.Backend.PGF.CmdLine' for more info.
--
-- The 'Surface' provides the necessary information for rendering PGF code and
-- building a PDF using "texrunner". See 'Diagrams.Backend.PGF.Surface'
-- for more info.
--

module Diagrams.Backend.PGF
  ( -- * Rendering token & options
    PGF (..)
  , B

    -- * Rendering functions
  , renderPGF
  , renderPGF'
  , renderPGFSurf

    -- * Options
    -- | Options for changing how the diagram is rendered. 'Options'
    --   'PGF' is an instance of 'Default':
    --
    -- @
    -- def = PGFOptions {
    --   _surface    = 'latexSurface'
    --   _sizeSpec   = 'absolute'
    --   _readable   = 'True'
    --   _standalone = 'False'
    --   }
    -- @
    --
    --   You can edit the default options using lenses.
  , readable
  , sizeSpec
  , surface
  , standalone

    -- * Surfaces
    -- | These surfaces should be suitable for basic diagrams. For more
    --   complicated options see 'Diagrams.Backend.PGF.Surface'.
  , Surface
  , surfOnlineTex

    -- ** Predefined surfaces
  , latexSurface
  , contextSurface
  , plaintexSurface

    -- ** Lenses
  , command
  , arguments
  , preamble

    -- * Online TeX
    -- | By using 'OnlineTex', diagrams is able to query tex for sizes
    --   of hboxs and give them the corresponding envelopes. These can
    --   then be used as any other diagram with the correct size.
    --
    --   Online diagrams use the 'Surface' to run tex in online mode and
    --   get feedback for hbox sizes. To run it you can use
    --   'renderOnlinePGF', 'renderOnlinePGF'' or 'onlineMain' from
    --   'Diagrams.Backend.PGF.CmdLine'.
    --
    --   See
    --   <https://github.com/diagrams/diagrams-pgf/tree/master/examples>
    --   for examples.
  , OnlineTex
  , renderOnlinePGF
  , renderOnlinePGF'

    -- ** Hbox
  , Hbox
  , hboxOnline
  , hboxPoint
  , hboxSurf
  ) where

import           Data.ByteString.Builder
import           System.Directory             hiding (readable)
import           System.FilePath
import           System.IO
import           System.Texrunner
import           System.Texrunner.Online      hiding (hbox)

import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy.Char8   as LB

import           Diagrams.Backend.PGF.Hbox
import           Diagrams.Backend.PGF.Render
import           Diagrams.Backend.PGF.Surface
import           Diagrams.Size
import           Diagrams.Prelude             hiding (r2)

type B = PGF

-- | Render a pgf diagram and write it to the given filepath. Same as
--   'renderPGF'' but uses the default options.
renderPGF :: (TypeableFloat n, Monoid' m)
          => FilePath         -- ^ path to output
          -> SizeSpec V2 n    -- ^ size of output
          -> QDiagram PGF V2 n m -- ^ 'Diagram' to render
          -> IO ()
renderPGF outFile sizeSp = renderPGFSurf outFile sizeSp def

-- | Render a pgf diagram and write it to the given filepath. Same as
--   'renderPGF' but takes a 'Surface'.
renderPGFSurf :: (TypeableFloat n, Monoid' m)
          => FilePath         -- ^ path to output
          -> SizeSpec V2 n    -- ^ size of output
          -> Surface          -- ^ surface to render with
          -> QDiagram PGF V2 n m -- ^ diagram to render
          -> IO ()
renderPGFSurf outFile sizeSp surf =
  renderPGF' outFile $
    def & sizeSpec .~ sizeSp
        & surface  .~ surf

-- | Render a pgf diagram and write it to the given filepath. If the file has
--   the extension @.pdf@, a PDF is generated in a temporary directory using
--   options from the given surface, otherwise, the tex output is saved
--   using the surface's 'TexFormat'.
renderPGF' :: (TypeableFloat n, Monoid' m)
           => FilePath -> Options PGF V2 n -> QDiagram PGF V2 n m -> IO ()
renderPGF' outFile opts d = case takeExtension outFile of
  ".pdf" -> do
    let rendered = renderDia PGF (opts & standalone .~ True & readable .~ False) d

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

-- | Render an online 'PGF' diagram and save it. Same as
--   'renderOnlinePGF'' using default options.
renderOnlinePGF :: (TypeableFloat n, Monoid' m)
                => FilePath
                -> SizeSpec V2 n
                -> OnlineTex (QDiagram PGF V2 n m)
                -> IO ()
renderOnlinePGF outFile sizeSp = renderOnlinePGF' outFile  (def & sizeSpec .~ sizeSp)

-- | Same as 'renderOnlinePDF' but takes 'Options' 'PGF'.
renderOnlinePGF' :: (TypeableFloat n, Monoid' m)
                 => FilePath
                 -> Options PGF V2 n
                 -> OnlineTex (QDiagram PGF V2 n m)
                 -> IO ()
renderOnlinePGF' outFile opts dOL = case takeExtension outFile of
  ".pdf" -> do

    ((), texLog, mPDF) <-
      runOnlineTex'
        (surf^.command)
        (surf^.arguments)
        (toByteString . stringUtf8 $ surf ^. (preamble <> beginDoc)) $ do

          d <- dOL

          -- we've already output the preamble so don't do it again
          let opts' = opts & surface    %~ set beginDoc "" . set preamble ""
                           & readable   .~ False
                           & standalone .~ True

              rendered = renderDia PGF opts' d

          texPutStrLn $ toByteString rendered

    case mPDF of
      Nothing  -> putStrLn "Error, no PDF found:"
               >> print texLog
      Just pdf -> LB.writeFile outFile pdf

  -- tex output
  _      ->  surfOnlineTexIO surf dOL >>= writeTexFile outFile opts
  where
    surf = opts ^. surface
    toByteString = LB.toStrict . toLazyByteString

-- | Write the rendered diagram to a text file, ignoring the extension.
writeTexFile :: (TypeableFloat n, Monoid' m)
             => FilePath -> Options PGF V2 n -> QDiagram PGF V2 n m -> IO ()
writeTexFile outFile opts d = do
  h <- openFile outFile WriteMode
  hPutBuilder h $ renderDia PGF opts d
  hClose h

