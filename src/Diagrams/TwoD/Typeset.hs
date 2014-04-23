{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Diagrams.TwoD.Typeset
  ( Typeset (..)
  , TypesetSize (..)
    -- * Lenses
  , rawText
  , tSize
  , tRotation
  , tAlign
  , tTrans
    -- * Diagrams
  , mkTypeset
  , typeset
  ) where

import Diagrams.Core
import Diagrams.Core.Envelope (pointEnvelope)
import Diagrams.TwoD.Text
import Diagrams.TwoD.Types
import Diagrams.Angle

import Data.Monoid
import Data.Typeable

import Control.Lens (makeLenses, (%~), (&))

-- | Sizes can either be the same as Text, where transformations are applied or 
--   Typeset sizes which match the sizes of the document. Typeset sizes are not 
--   affected by transformations directly, rotations are applied with the 
--   'tRotation' lens.
data TypesetSize = DiagramsSize Double
                 | Tiny
                 | Small
                 | Medium
                 | Large
                 | Huge
                 | PtSize Double
                 deriving (Typeable, Show)


-- | Typeset is for backends with more advanced text support and is required 
--   for formulas. This type is not intended to be used directly. Still 
--   undecided how this should be implemented. (Currently inherits same 
--   FontSlant and FontWeight as Text, will probably change.)
data Typeset = Typeset
  { _rawText   :: String
  , _tSize     :: TypesetSize
  , _tRotation :: Angle
  , _tAlign    :: TextAlignment
  , _tTrans    :: Transformation R2
  -- , _boxWidth :: Maybe Double
  } deriving Typeable

makeLenses ''Typeset

type instance V Typeset = R2

instance Transformable Typeset where
  transform t typ = typ & tTrans %~ (t <>)

-- instance IsPrim Typeset

instance Renderable Typeset NullBackend where
  render _ _ = mempty

mkTypeset :: Renderable Typeset b
          => Typeset -> Diagram b R2

mkTypeset tps = mkQD (Prim tps)
                     (pointEnvelope origin)
                     mempty
                     mempty
                     mempty

typeset :: String -> Typeset
typeset str = Typeset str Medium (0@@rad) (BoxAlignedText 0.5 0.5) mempty

