{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.PGF
-- Copyright   :  (c) 2014 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A hbox a primitive Tex box, typically used for holding text and
-- formulas but can hold anything. This module provides functions for
-- retrieving the dimensions of these boxes to give diagrams the correct
-- envelopes.
-----------------------------------------------------------------------------
module Diagrams.Backend.PGF.Hbox
  ( Hbox (..)
  , _Hbox

    -- * Enveloped diagrams
    -- | The dimensions of a hbox can be recovered by calling Tex. The
    --   resulting envelope has its origin at the baseline of the text.
    --
    --   <<diagrams/hbox.svg#width=200 hbox>>
  , hboxOnline

   -- ** Non-Online version
   -- | These versions bypass 'OnlineTex' by just running a whole tex
   --   program just to get the size of a single hbox. This is not
   --   recommended but because it is slow, but can be convientient if
   --   you only need one or two hbox sizes.
  , hboxSurf
  , hboxSurfIO

    -- * Point envelope diagrams
  , hboxPoint

  ) where

import           Data.ByteString.Char8        (pack)
-- import           Data.Monoid
import           Data.Typeable
import           System.IO.Unsafe
import           System.Texrunner.Online      hiding (hbox)
import qualified System.Texrunner.Online      as Online
import           System.Texrunner.Parse

import           Geometry.Envelope
import           Geometry.Trace
import           Geometry.Query
-- import           Geometry.Transform
import           Geometry.TwoD.Types
import           Geometry.Points
import           Geometry.BoundingBox
import           Geometry.Space
-- import           Diagrams.Prelude             hiding (Box, (<>))
import Diagrams.Types
import Control.Lens (Prism')

import           Diagrams.Backend.PGF.Surface

-- | Primitive for placing raw Tex commands in a hbox.
data Hbox n = Hbox String
  deriving Typeable

_Hbox :: (Typeable n, Num n) => Prism' (Prim V2 n) (Hbox n)
_Hbox = _Prim

type instance V (Hbox n) = V2
type instance N (Hbox n) = n

-- | Raw Tex commands in a hbox with no envelope. Transformations are
-- applied normally. This primitive ignores
-- 'Diagrams.TwoD.Text.FontSize'.
hboxPoint :: String -> Diagram V2
hboxPoint raw = mkQD (Prim (Hbox raw))
                (pointEnvelope origin)
                mempty
                mempty

-- | Hbox with bounding box envelope. Note that each box requires a call to
--   Tex. For multiple boxes consider using 'onlineHbox' to get multiple boxes
--   from a single call. (uses unsafePerformIO)
hboxSurf :: Surface -> String -> Diagram V2
hboxSurf surf txt = unsafePerformIO (hboxSurfIO surf txt)
{-# NOINLINE hboxSurf #-}

-- | Hbox with bounding box envelope. Note that each box requires a call to
--   Tex. For multiple boxes consider using 'onlineHbox' to get multiple boxes
--   from a single call.
hboxSurfIO :: Surface -> String -> IO (Diagram V2)
hboxSurfIO surf txt = surfOnlineTexIO surf (hboxOnline txt)

-- | Hbox with bounding box envelope.
hboxOnline :: String -> OnlineTex (Diagram V2)
hboxOnline txt = do
  Box h d w <- Online.hbox (pack txt)

  let bb = fromCorners (P $ V2 0 (-d))
                       (P $ V2 w h)

  return $ mkQD (Prim (Hbox txt))
                (getEnvelope bb)
                (getTrace bb)
                (getQuery bb)

