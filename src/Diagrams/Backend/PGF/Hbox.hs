{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Diagrams.Backend.PGF.Hbox
  ( Hbox (..)
  , hbox
  ) where

import Diagrams.Core
import Diagrams.Core.Envelope (pointEnvelope)
import Diagrams.TwoD.Types

import Data.Monoid
import Data.Typeable


-- | Data type typing raw TeX commands in a hbox.
data Hbox = Hbox (Transformation R2) String
  deriving Typeable

type instance V Hbox = R2

instance Transformable Hbox where
  transform tr (Hbox tr' str)
    = Hbox (tr' <> tr) str

instance Renderable Hbox NullBackend where
  render _ _ = mempty

-- | Used to insert raw TeX commands into a hbox, traslations are 
--   applied from the transformations.
hbox :: Renderable Hbox b => String -> Diagram b R2
hbox raw = mkQD (Prim (Hbox mempty raw))
                (pointEnvelope origin)
                mempty
                mempty
                mempty


