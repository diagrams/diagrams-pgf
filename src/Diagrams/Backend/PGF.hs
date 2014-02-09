{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
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
      -- ** Options lenses
    , template, surface, sizeSpec, readable
    , SizeSpec2D(..)
    , module Diagrams.Backend.PGF.Surface
    , defaultSurface
      -- * Rendering functions
    , renderDia
    , renderPGF
    , renderPDF
    , sizeSpecToBounds
    ) where

import           Control.Lens ((^.))
import           Data.Default
import           Diagrams.Prelude              hiding (r2, view)
import           System.Exit
import           System.FilePath
import           System.Process
import           System.IO
import qualified Blaze.ByteString.Builder      as Blaze
import qualified Data.ByteString.Char8         as B

import           Diagrams.Backend.PGF.Render
import           Diagrams.Backend.PGF.Surface


type B = PGF

-- |
-- @
--  defaultSurface = 'PGFSurface'
-- @
defaultSurface :: Surface
defaultSurface = def

-- | Render a diagram as a PGF code, writing to the specified output
--   file and using the requested size and surface.
renderPGF :: FilePath -> SizeSpec2D -> Surface -> Diagram PGF R2 -> IO ()
renderPGF filePath sizeSp surf dia = do
    h <- openFile filePath WriteMode
    let rendered = renderDia PGF (def & template .~ surf & sizeSpec .~ sizeSp ) dia
    Blaze.toByteStringIO (B.hPutStr h) rendered
    hClose h

-- | Render PGF and save to output.tex and run:
--
-- > pdflatex -interaction=batchmode "output.tex"
--
--   for LaTeX or
--
-- > context --batch --once "output.tex"
--
--   for ConTeXt or
--
-- > pdftex -interaction=batchmode "output.tex"
--
--   stdout is hidden but errors are notified and "output.log" is kept.
--
renderPDF :: FilePath -> SizeSpec2D -> Surface -> Diagram PGF R2 -> IO ()
renderPDF filePath sizeSp surf dia = do
    let texFilePath = replaceExtension filePath "tex"
        outDir      = dropFileName filePath
    renderPGF texFilePath sizeSp surf dia
    --
    let (cmd,args) = case surf^.texFormat of
          LaTeX    -> ("pdflatex", [ "--output-directory=" ++ outDir
                                   , "--batch"
                                   , texFilePath])
          ConTeXt  -> ("context", ["--batch", "--noconsole", texFilePath])
          PlainTeX -> ("pdftex", [texFilePath])
    --
    (ecode, _{- out -}, _{- err -}) <- readProcessWithExitCode cmd args ""
    
    case ecode of
      ExitFailure _ ->
        putStrLn $ "An error occured while processing tex file, please check "
              ++ replaceExtension filePath "log"
              ++ " and report if this is a bug."
      ExitSuccess   -> return ()



