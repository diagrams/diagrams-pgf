{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.PGF
-- Copyright   :  (c) 2014 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  c.chalmers@me.com
--
-- A hbox a primitive TeX box, typically used for holding text. This module
-- provides functions for retrieving the dimensions of these boxes so they may be
-- used as envelopes for diagrams.
-----------------------------------------------------------------------------
module Diagrams.Backend.PGF.Hbox
  ( Hbox (..)
    -- * Enveloped diagrams
    -- | The dimensions of a hbox can be recovered by calling TeX. The
    --   resulting envelope has its origin at the baseline of the text.
    --
    --   <<diagrams/hbox.svg#width=200 hbox>>
  , onlineHbox
  , surfOnlineTex
  , surfaceHbox
    -- * Point envelope diagrams
  , hbox
  , hboxInv
    -- * IO versions
    -- | If used properly, the non-IO versions should be \'safe\'. However we
    --   supply the IO versions anyway.
  , hboxIO
  , surfOnlineTexIO
  ) where

import           Control.Lens                     ((^.))
import           Data.ByteString.Char8            (pack)
import           Data.Monoid
import           Data.Typeable
import           System.IO.Unsafe
import           System.TeXRunner.Online          hiding (hbox)
import qualified System.TeXRunner.Online          as Online
import           System.TeXRunner.Parse

import           Diagrams.BoundingBox
import           Diagrams.Core
import           Diagrams.Core.Envelope           (pointEnvelope)
import           Diagrams.Points
import           Diagrams.TwoD
import           Diagrams.TwoD.Transform.ScaleInv

import           Diagrams.Backend.PGF.Surface

-- | Primitive for placing raw TeX commands in a hbox.
data Hbox n = Hbox (Transformation V2 n) String
  deriving Typeable

type instance V (Hbox n) = V2
type instance N (Hbox n) = n

instance Fractional n => Transformable (Hbox n) where
  transform t (Hbox tt str)
    = Hbox (t <> tt) str

instance Fractional n => Renderable (Hbox n) NullBackend where
  render _ _ = mempty

-- | Raw TeX commands with no envelope. Transformations are applied normally.
hbox :: (Fractional n, Ord n, Typeable n, Renderable (Hbox n) b)
     => String -> Diagram b V2 n
hbox raw = mkQD (Prim (Hbox mempty raw))
                (pointEnvelope origin)
                mempty
                mempty
                mempty

-- | Raw TeX commands with no envelope. Only translational transformations are
--   applied.
hboxInv :: (RealFloat n, Typeable n, Renderable (Hbox n) b)
        => String -> Diagram b V2 n
hboxInv txt = scaleInvPrim (Hbox mempty txt) (mkR2 0 0)

-- | Hbox with bounding box envelope. Note that each box requires a call to
--   TeX. For multiple boxes consider using 'onlineHbox' to get multiple boxes
--   from a single call. (uses unsafePerformIO)
surfaceHbox :: (RealFloat n, Typeable n, Renderable (Hbox n) b)
            => Surface -> String -> Diagram b V2 n
surfaceHbox surf txt = unsafePerformIO (hboxIO surf txt)
{-# NOINLINE surfaceHbox #-}

-- | Hbox with bounding box envelope. Note that each box requires a call to
--   TeX. For multiple boxes consider using 'onlineHbox' to get multiple boxes
--   from a single call.
hboxIO :: (RealFloat n, Typeable n, Renderable (Hbox n) b)
       => Surface -> String -> IO (Diagram b V2 n)
hboxIO surf txt = surfOnlineTexIO surf (onlineHbox txt)

-- | Get the result of an OnlineTeX using the given surface.
surfOnlineTex :: Surface -> OnlineTeX a -> a
surfOnlineTex surf a = unsafePerformIO (surfOnlineTexIO surf a)
{-# NOINLINE surfOnlineTex #-}

-- | Get the result of an OnlineTeX using the given surface.
surfOnlineTexIO :: Surface -> OnlineTeX a -> IO a
surfOnlineTexIO surf = runOnlineTex (surf^.command)
                                    (surf^.arguments)
                                    (pack $ surf^.preamble ++ surf^.beginDoc)

-- | Hbox with bounding box envelope.
onlineHbox :: (RealFloat n, Typeable n, Renderable (Hbox n) b)
           => String -> OnlineTeX (Diagram b V2 n)
onlineHbox txt = do
  (Box h d w) <- Online.hbox (pack txt)

  let bb = fromCorners (P $ V2 0 (-d))
                       (P $ V2 w h)

  return $ mkQD (Prim (Hbox mempty txt))
                (getEnvelope bb)
                (getTrace bb)
                mempty
                (boundingBoxQuery bb)

