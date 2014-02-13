-----------------------------------------------------------------------------
-- |
-- This module provides functions for querying TeX for the envelope sizes of 
-- Text and Typeset formulas. This involves delicate interfacing with TeX and 
-- should be considered very experimental.
--
----------------------------------------------------------------------------


module Diagrams.Backend.PGF.Typeset
  ( TeXProcess
  , mkTeXProcess
  , closeTeXProcess
  , texEnvelope
  ) where

import System.Process
import System.IO

import Diagrams.Core.Envelope
import Diagrams.TwoD.Types (R2)
-- import Diagrams.TwoD.Typeset
import Diagrams.Backend.PGF.Surface

-- | The TeXProcess should always be in a state where it is ready to accept 
--   queries and print them to it's out handle.
data TeXProcess = TeXProcess
  { texProcessHandle :: ProcessHandle
  , texInHandle      :: Handle
  , texOutHandle     :: Handle
  }

-- | Uses a surface to open an interface with TeX, 
mkTeXProcess :: Surface -> IO TeXProcess
mkTeXProcess = undefined
-- call TeX
-- run premable
-- run \tracingonline=1
-- return

closeTeXProcess :: TeXProcess -> IO ()
closeTeXProcess = undefined


texEnvelope :: TeXProcess -> String -> IO (Envelope R2, TeXProcess)
texEnvelope = undefined
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
-- parse this
-- calculate envelope
--
-- \n
-- flush
-- return envelope and TeXProcess

