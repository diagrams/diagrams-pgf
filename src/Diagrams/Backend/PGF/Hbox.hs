{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Diagrams.Backend.PGF.Hbox
  ( Hbox (..)
  , hbox
  , hboxEmpty
  , hboxP
  , surfTexProcess
    -- * IO versions
  , hboxIO
  , surfTexProcessIO
  ) where

import Control.Lens ((^.))
import Data.ByteString.Char8 (pack)
import Data.Monoid
import Data.Typeable
import System.IO.Unsafe
import System.TeXRunner.Online hiding (hbox)
import System.TeXRunner.Parse

import qualified System.TeXRunner.Online as Online

import Diagrams.Prelude hiding ((<>))

import Diagrams.BoundingBox
import Diagrams.Core.Envelope (pointEnvelope)

import Diagrams.Backend.PGF.Surface

-- | Data type typing raw TeX commands in a hbox.
data Hbox = Hbox (Transformation R2) (Transformation R2) String
  deriving Typeable

type instance V Hbox = R2

instance Transformable Hbox where
  transform t (Hbox tt tn str)
    = Hbox (t <> tt) (t <> tn <> scaling (1 / avgScale t)) str

instance Renderable Hbox NullBackend where
  render _ _ = mempty

-- | Used to insert raw TeX commands into a hbox, traslations are 
--   applied from the transformations.
hboxEmpty :: Renderable Hbox b => String -> Diagram b R2
hboxEmpty raw = mkQD (Prim (Hbox mempty mempty raw))
                (pointEnvelope origin)
                mempty
                mempty
                mempty

hbox :: Renderable Hbox b => Surface -> String -> Diagram b R2
hbox surf txt = unsafePerformIO (hboxIO surf txt)
{-# NOINLINE hbox #-}

hboxIO :: Renderable Hbox b => Surface -> String -> IO (Diagram b R2)
hboxIO surf txt = do
  box <- runTexProcess (surf^.command)
                       (surf^.arguments)
                       (pack $ surf^.preamble)
                       (Online.hbox $ pack txt)

  let env = getEnvelope
          $ fromCorners (mkP2 0 (negate $ boxDepth box))
                        (mkP2 (boxWidth box) (boxHeight box))

  return $ mkQD (Prim (Hbox mempty mempty txt))
                env
                mempty
                mempty
                mempty

surfTexProcess :: Surface -> TeXProcess a -> a
surfTexProcess surf a = unsafePerformIO (surfTexProcessIO surf a)
{-# NOINLINE surfTexProcess #-}

surfTexProcessIO :: Surface -> TeXProcess a -> IO a
surfTexProcessIO surf = runTexProcess (surf^.command)
                                      (surf^.arguments)
                                      (pack $ surf^.preamble)

hboxP :: Renderable Hbox b => String -> TeXProcess (Diagram b R2)
hboxP txt = do
  (Box h d w) <- Online.hbox (pack txt)

  let env = getEnvelope
          $ fromCorners (mkP2 0 (negate d))
                        (mkP2 w h)

  return $ mkQD (Prim (Hbox mempty mempty txt))
                env
                mempty
                mempty
                mempty
