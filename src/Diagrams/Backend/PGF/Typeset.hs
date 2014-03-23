{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- This module provides functions for querying TeX for the envelope sizes of 
-- Text and Typeset formulas. This involves delicate interfacing with TeX and 
-- should be considered very experimental.
--
----------------------------------------------------------------------------


module Diagrams.Backend.PGF.Typeset
  ( TeXProcess (..)
  , Box (..)
  , mkTeXProcess
  , closeTeXProcess
  -- , texEnvelope
  --
  , parseBox
  , strToBox
  , diagramText
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Lens
import System.Directory
import System.Process
import System.IO

import Data.Attoparsec.ByteString.Char8 as A
-- import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Diagrams.Core.Types
import Diagrams.Envelope
import Diagrams.TwoD.Align
import Diagrams.TwoD.Types (R2)
import Diagrams.TwoD.Text
import Diagrams.TwoD.Transform
import Diagrams.TwoD.Shapes
-- import Diagrams.TwoD.Typeset
import Diagrams.Backend.PGF.Surface
import Diagrams.Backend.PGF  -- .Render


-- | The TeXProcess should always be in a state where it is ready to accept 
--   queries and print them to it's out handle.
data TeXProcess = TeXProcess
  { texProcessHandle :: ProcessHandle
  , texInHandle      :: Handle
  , texOutHandle     :: Handle
  }

data Box = Box
  { boxHeight :: Double
  , boxDepth  :: Double
  , boxWidth  :: Double
  } deriving Show

-- | Uses a surface to open an interface with TeX, 
mkTeXProcess :: Surface -> IO TeXProcess
mkTeXProcess surf = do
  tmp <- getTemporaryDirectory
  let jobName = "typeset"
      args = (surf^.jobArg) jobName : surf^.arguments
      p = (proc (surf^.command) args)
            { cwd     = Just tmp
            , std_in  = CreatePipe
            , std_out = CreatePipe
            }
  --
  (Just inH, Just outH, _, pHandle) <- createProcess p

  hSetBuffering inH LineBuffering -- needed for TeX
  hPutStrLn inH $ surf^.preamble
  hPutStrLn inH $ surf^.beginDoc
  hPutStrLn inH "\\tracingonline=1"
  hPutStrLn inH "\\showboxdepth=1"
  hPutStrLn inH "\\showboxbreadth=1"

  return $ TeXProcess pHandle inH outH

closeTeXProcess :: TeXProcess -> IO ()
closeTeXProcess (TeXProcess pHandle inH outH) = do
  hClose inH
  _ <- waitForProcess pHandle
  hClose outH

  
strToBox :: TeXProcess -> String -> IO Box
strToBox (TeXProcess _ inH outH) str = do
  _ <- B.hGetNonBlocking outH 10000
  hPutStrLn inH $ "\\setbox0=\\hbox{" ++ str ++ "}"
  hPutStrLn inH "\\showbox0\n"
  threadDelay 10000
  getBox outH

getBox :: Handle -> IO Box
getBox h = do
  str <- B.hGetNonBlocking h 5000
  -- B.putStrLn $ "I'm about to parse:\n" <> str
  return $ case parseOnly parseBox str of
             Right box -> box
             Left _    -> Box 0 0 0

parseBox :: Parser Box
parseBox = do
  A.skipWhile (/= '(') <* char '('
  h <- double <* char '+'
  d <- double <* char ')' <* char 'x'
  w <- double
  --
  return $ Box (h/8) (d/8) (w/8)

combineBoxString :: Box -> String -> Diagram PGF R2
combineBoxString (Box h d w) str = withEnvelope rec $ baselineText str
  where
    rec = translateY (-d) . alignBL $ rect w (h+d) :: Diagram PGF R2

diagramText :: Surface -> String -> IO (Diagram PGF R2)
diagramText surf str = do
  p <- mkTeXProcess surf
  threadDelay 1000000
  box <- strToBox p str
  closeTeXProcess p
  return $ combineBoxString box str


-- \setbox0=\hbox{<<string>>}
-- \showbox0
-- >   *\showbox0
-- >   > \box0=
------------ height  depth    width
-- >   \hbox(6.83331+2.15277)x18.6108
-- >   .\tenrm T
-- >   .\kern -1.66702
-- >   .\hbox(6.83331+0.0)x6.80557, shifted 2.15277
-- >   ..\tenrm E
-- >   .\kern -1.25
-- >   .\tenrm X
-- >   
-- >   ! OK.
-- >   <*> \showbox0
-- >                
-- >   ? 
-- 
