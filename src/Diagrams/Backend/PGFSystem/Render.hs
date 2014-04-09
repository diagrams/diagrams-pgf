{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Diagrams.Backend.PGFSystem.Render
  ( PGFSystem (..)
  , sizeSpec
  , surface
  , Options (..)
  ) where

import qualified Blaze.ByteString.Builder as Blaze
import           Control.Lens             (lens, op, Lens', (^.))
import           Control.Monad.State
import           Data.Default
import           Data.Maybe               (isJust)
import           Data.Typeable
import           Data.Foldable            (foldMap)
import           Data.Hashable            (Hashable (..))
import           Data.Tree

import           Diagrams.Prelude
-- import           Diagrams.TwoD.Image
import           Diagrams.TwoD.Adjust     (adjustDiaSize2D)
import           Diagrams.Core.Compile
import           Diagrams.Core.Types      (Annotation)
import           Diagrams.TwoD.Path
-- import           Diagrams.TwoD.Text

import           Diagrams.Backend.PGF.Hbox
import qualified Graphics.Rendering.PGFSystem as P
import           Diagrams.Backend.PGFSystem.Surface

data PGFSystem = PGFSystem
  deriving (Show, Typeable)

instance Backend PGFSystem R2 where
  data Render  PGFSystem R2 = P P.Put
  type Result  PGFSystem R2 = Blaze.Builder
  data Options PGFSystem R2 = PGFOptions
      { _surface    :: Surface    -- ^ Surface you want to use.
      , _sizeSpec   :: SizeSpec2D -- ^ The requested size.
      , _standalone :: Bool       -- ^ Should include preamble etc.
      }
  renderRTree _ ops rt =
    P.renderWith (_surface ops) (_standalone ops) bounds r
    where
      (P r)  = toRender rt
      bounds = sizeSpecToBounds (ops^.sizeSpec)

  adjustDia = adjustDiaSize2D sizeSpec

instance Monoid (Render PGFSystem R2) where
  mempty                  = P $ return ()
  (P ra) `mappend` (P rb) = P (ra >> rb)

toRender :: RTree PGFSystem R2 Annotation -> Render PGFSystem R2
toRender = fromRTree . splitFills
  where
    -- fromRTree (Node (TransparencyGroup x) rs)
    --   = P $ do
    --       let R r = foldMap fromRTree rs
    --       pgf <- r
    --       return $ P.transparencyGroup x pgf
    fromRTree (Node (RPrim p) _) = render PGFSystem p
    fromRTree (Node (RStyle sty) rs)
      = P . P.scope $ do
          let P r = foldMap fromRTree rs
          style sty
          r
          draw sty
    fromRTree (Node _ rs) = foldMap fromRTree rs

sizeSpecToBounds :: SizeSpec2D -> (Double, Double)
sizeSpecToBounds spec = case spec of
   Width w  -> (w,w)
   Height h -> (h,h)
   Dims w h -> (w,h)
   Absolute -> (100,100)

instance Default (Options PGFSystem R2) where
  def = PGFOptions
          { _surface    = def
          , _sizeSpec   = Absolute
          , _standalone = True
          }

sizeSpec :: Lens' (Options PGFSystem R2) SizeSpec2D
sizeSpec = lens getSize setSize
  where getSize (PGFOptions { _sizeSpec = s }) = s
        setSize o s = o { _sizeSpec = s }

surface :: Lens' (Options PGFSystem R2) Surface
surface = lens getsurface setsurface
  where getsurface (PGFOptions { _surface = s }) = s
        setsurface o s = o { _surface = s }

style :: Style R2 -> P.Put
style s = do
  P.fillColor <*~ getFillColor
  P.fillRule  <~  getFillRule
  --
  P.lineColor <*~ getLineColor
  P.lineJoin  <~  getLineJoin
  P.lineCap   <~  getLineCap
  P.dash      <~  getDashing
  P.lineWidth <~  fromOutput . getLineWidth
  where
  (<~) :: (AttributeClass a) => (b -> P.Put) -> (a -> b) -> P.Put
  setter <~ getter = maybe (return ()) setter mAttribute
    where mAttribute = (getter <$>) . getAttr $ s
  infixr 2 <~
  --
  (<*~) :: (AttributeClass a, Color c)
        => (AlphaColour Double -> P.Put) -> (a -> c) -> P.Put
  setColor <*~ getColor = (setColor . fade . toAlphaColour) <~ getColor
  --
  fade = dissolve $ maybe 1 getOpacity (getAttr s)

shouldStroke :: Style R2 -> Bool
shouldStroke s = maybe True (> P.epsilon) mLineWidth
  where
    mLineWidth = (fromOutput . getLineWidth <$>) . getAttr $ s

shouldFill :: Style R2 -> Bool
shouldFill s = isJust mFillColor
  where
    mFillColor = (getFillColor <$>) . getAttr $ s

draw :: Style R2 -> P.Put
draw s = case (shouldFill s, shouldStroke s) of
           (True, True)   -> P.fillStroke
           (True, False)  -> P.fill
           (False, True)  -> P.stroke
           (False, False) -> return ()

------------------------------------------------------------------------
-- Renderable instances

-- instance Renderable (Segment Closed R2) PGFSystem where
--   render b = render b . (fromSegments :: [Segment Closed R2] -> Path R2) . (:[])
-- 
-- instance Renderable (Trail R2) PGFSystem where
--   render b = render b . pathFromTrail
-- 
instance Renderable (Path R2) PGFSystem where
  render _ = P . P.path
  -- render _ p = P $ do
  --   -- dirty hack, but we avoid needing state and how often to people try to 
  --   -- fill lines?
  --   when (any (isLine . unLoc) . op Path $ p) $ P.fillOpacity 0
  --   P.path p

instance Renderable Hbox PGFSystem where
  render _ = P . P.hbox

------------------------------------------------------------------------
-- Hashable instances

instance Hashable (Options PGFSystem R2) where
  hashWithSalt s (PGFOptions sf sz st)
    = s  `hashWithSalt`
      sf `hashWithSalt`
      sz `hashWithSalt`
      st
