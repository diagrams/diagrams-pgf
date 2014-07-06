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

import Control.Lens            ((^.))
import Data.ByteString.Char8   (pack)
import Data.Monoid
import Data.Typeable
import System.IO.Unsafe
import System.TeXRunner.Online hiding (hbox)
import System.TeXRunner.Parse

import qualified System.TeXRunner.Online as Online


import Diagrams.BoundingBox
import Diagrams.Core.Envelope           (pointEnvelope)
import Diagrams.Prelude                 hiding ((<>))
import Diagrams.TwoD.Transform.ScaleInv

import Diagrams.Backend.PGF.Surface

-- | Primitive for placing raw TeX commands in a hbox.
data Hbox = Hbox (Transformation R2) String
  deriving Typeable

type instance V Hbox = R2

instance Transformable Hbox where
  transform t (Hbox tt str)
    = Hbox (t <> tt) str

instance Renderable Hbox NullBackend where
  render _ _ = mempty

-- | Raw TeX commands with no envelope. Transformations are applied normally.
hbox :: Renderable Hbox b => String -> Diagram b R2
hbox raw = mkQD (Prim (Hbox mempty raw))
                (pointEnvelope origin)
                mempty
                mempty
                mempty

-- | Raw TeX commands with no envelope. Only translational transformations are
--   applied.
hboxInv :: Renderable Hbox b => String -> Diagram b R2
hboxInv txt = scaleInvPrim (Hbox mempty txt) (mkR2 0 0)

-- | Hbox with bounding box envelope. Note that each box requires a call to
--   TeX. For multiple boxes consider using 'onlineHbox' to get multiple boxes
--   from a single call. (uses unsafePerformIO)
surfaceHbox :: Renderable Hbox b => Surface -> String -> Diagram b R2
surfaceHbox surf txt = unsafePerformIO (hboxIO surf txt)
{-# NOINLINE surfaceHbox #-}

-- | Hbox with bounding box envelope. Note that each box requires a call to
--   TeX. For multiple boxes consider using 'onlineHbox' to get multiple boxes
--   from a single call.
hboxIO :: Renderable Hbox b => Surface -> String -> IO (Diagram b R2)
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
onlineHbox :: Renderable Hbox b => String -> OnlineTeX (Diagram b R2)
onlineHbox txt = do
  (Box h d w) <- Online.hbox (pack txt)

  let env = getEnvelope
          $ fromCorners (0 ^& negate d)
                        (w ^& h)

  return $ mkQD (Prim (Hbox mempty txt))
                env
                mempty
                mempty
                mempty

