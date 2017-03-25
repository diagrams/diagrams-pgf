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

    -- * Enveloped diagrams
    -- | The dimensions of a hbox can be recovered by calling Tex. The
    --   resulting envelope has its origin at the baseline of the text.
    --
    --   <<diagrams/hbox.svg#width=200 hbox>>
  , hboxOnline

   -- ** Non-Online version
   -- | These versions bypass 'OnlineTex' by just running a whole tex
   --   program just to get the size of a single hbox. This is not
   --   recommended but because it is slow, but can be convenient if
   --   you only need one or two hbox sizes.
  , hboxSurf
  , hboxSurfIO

    -- * Point envelope diagrams
  , hboxPoint

  ) where

import           Data.ByteString.Lazy         (toStrict)
import           Data.ByteString.Builder      (stringUtf8, toLazyByteString)
import           Data.Monoid
import           Data.Typeable
import           System.IO.Unsafe
import           System.Texrunner.Online      hiding (hbox)
import qualified System.Texrunner.Online      as Online
import           System.Texrunner.Parse

import           Diagrams.Core.Envelope       (pointEnvelope)
import           Diagrams.Prelude             hiding (Box, (<>))

import           Diagrams.Backend.PGF.Surface

-- | Primitive for placing raw Tex commands in a hbox.
data Hbox n = Hbox (Transformation V2 n) String
  deriving Typeable

type instance V (Hbox n) = V2
type instance N (Hbox n) = n

instance Fractional n => Transformable (Hbox n) where
  transform t (Hbox tt str) = Hbox (t <> tt) str

instance Fractional n => Renderable (Hbox n) NullBackend where
  render _ _ = mempty

-- | Raw Tex commands in a hbox with no envelope. Transformations are
-- applied normally. This primitive ignores
-- 'Diagrams.TwoD.Text.FontSize'.
hboxPoint :: (OrderedField n, Typeable n, Renderable (Hbox n) b)
     => String -> QDiagram b V2 n Any
hboxPoint raw = mkQD (Prim (Hbox mempty raw))
                (pointEnvelope origin)
                mempty
                mempty
                mempty

-- | Hbox with bounding box envelope. Note that each box requires a call to
--   Tex. For multiple boxes consider using 'hboxOnline' to get multiple boxes
--   from a single call. (uses unsafePerformIO)
hboxSurf :: (TypeableFloat n, Renderable (Hbox n) b)
            => Surface -> String -> QDiagram b V2 n Any
hboxSurf surf txt = unsafePerformIO (hboxSurfIO surf txt)
{-# NOINLINE hboxSurf #-}

-- | Hbox with bounding box envelope. Note that each box requires a call to
--   Tex. For multiple boxes consider using 'hboxOnline' to get multiple boxes
--   from a single call.
hboxSurfIO :: (TypeableFloat n, Renderable (Hbox n) b)
       => Surface -> String -> IO (QDiagram b V2 n Any)
hboxSurfIO surf txt = surfOnlineTexIO surf (hboxOnline txt)

-- | Hbox with bounding box envelope.
hboxOnline :: (TypeableFloat n, Renderable (Hbox n) b)
           => String -> OnlineTex (QDiagram b V2 n Any)
hboxOnline txt = do
  Box h d w <- Online.hbox (toStrict . toLazyByteString $ stringUtf8 txt)

  let bb = fromCorners (P $ V2 0 (-d))
                       (P $ V2 w h)

  return $ mkQD (Prim (Hbox mempty txt))
                (getEnvelope bb)
                (getTrace bb)
                mempty
                (getQuery bb)

