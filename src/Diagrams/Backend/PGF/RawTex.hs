{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Diagrams.Backend.PGF.RawTeX
  ( RawTeX (..)
  , tex
  ) where

import Diagrams.Attributes
import Diagrams.Core
import Diagrams.Core.Envelope (pointEnvelope)
import Diagrams.TwoD.Types

import Data.Monoid
import Data.Colour
import Data.Typeable

import Control.Lens (makeLenses, (%~), (&))


-- | Data type for holding raw TeX commands.
data RawTeX = RawTeX
  { rawTeX       :: String
  , _rawTeXTrans :: Transformation R2
  } deriving Typeable

makeLenses ''RawTeX

type instance V RawTeX = R2

instance Transformable RawTeX where
  transform t raw = raw & rawTeXTrans %~ (t <>)

instance IsPrim RawTeX

instance Renderable RawTeX NullBackend where
  render _ _ = mempty

-- | Used to insert raw TeX commands, like math mode and font changes.
tex :: Renderable RawTeX b => String -> Diagram b R2
tex raw = recommendFillColor (black :: Colour Double)
              $ mkQD (Prim (RawTeX raw mempty))
                     (pointEnvelope origin)
                     mempty
                     mempty
                     mempty

