{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Diagrams.Backend.PGF.Render.Sys
  ( PGFSys (..)
  , B
  -- , StateBuilder
  -- , initialRenderState
  , Options (..)
  ) where

import           Control.Lens              (lens, op, view, set, Lens')
import           Control.Monad.State
-- import           Data.Default
import           Data.Maybe                (isJust)
import           Data.Typeable
import           Diagrams.Prelude          hiding (r2, view, opacity)
-- import           Diagrams.TwoD.Image
import           Diagrams.TwoD.Adjust      (adjustDiaSize2D)
import           Diagrams.TwoD.Path
-- import           Diagrams.TwoD.Text
import qualified Blaze.ByteString.Builder  as Blaze

import qualified Graphics.Rendering.PGF.Sys    as P
import           Diagrams.Backend.PGF.Surface

data PGFSys = PGFSys
  deriving (Show, Typeable)

-- type StateBuilder = State RenderState P.Put
-- 
-- initialRenderState :: RenderState
-- initialRenderState = RenderState False

type B = PGFSys

-- -- | Convert an RTree to a renderable object. The unfrozen transforms have
-- --   been accumulated and are in the leaves of the RTree along with the Prims.
-- --   Frozen transformations have their own nodes and the styles have been
-- --   transfomed during the contruction of the RTree.
-- renderRTree :: RTree SVG R2 a -> Render SVG R2
-- renderRTree (Node (RPrim accTr p) _) = (render SVG (transform accTr p))
-- renderRTree (Node (RStyle sty) ts)
--   = R $ do
--       let P r = foldMap renderRTree ts
--       -- ignoreFill .= False
--       svg <- r
--       -- ign <- use ignoreFill
--       clippedSvg <- renderSvgWithClipping svg sty
--       return $ (S.g ! R.renderStyles ign sty) clippedSvg
-- renderRTree (Node (RFrozenTr tr) ts)
--   = P $ do
--       let P r = foldMap renderRTree ts
--       svg <- r
--       return $ P.transform tr
-- renderRTree (Node _ ts) = foldMap renderRTree ts


instance Backend PGFSys R2 where
  -- data Render  PGFSys R2 = P StateBuilder
  data Render  PGFSys R2 = P P.Put
  type Result  PGFSys R2 = Blaze.Builder
  data Options PGFSys R2 = PGFOptions
      { _template :: Surface    -- ^ Surface you want to use.
      , _sizeSpec :: SizeSpec2D -- ^ The requested size.
      }
-- 
  withStyle _ s t (P r) = P $
    P.scope $ do
      P.transform t
      style s
      maybe (return ()) P.clip $ getAttr s
      r
      draw s
  --   --   -- P.style %= (<> s)
  --   --   -- setClipPaths <~ op Clip
  --     pgf
  -- withStyle = error "style"
  -- withStyle _ s t (P r) = P $ do
  --   pgf <- r
  --   return . P.scope $ do
  --     P.transform t
  -- --   --   -- P.style %= (<> s)
  -- --   --   -- setClipPaths <~ op Clip
  --     pgf
  --     P.raw "yo"

-- 
  -- doRender _ _ (P r) = P.render $ evalState r initialRenderState
  doRender _ _ (P r) = P.render (P.picture r)
    -- where pgfsys = liftM P.render r
            -- pgf <- r
            -- return $ P.render pgf
--       -- P.renderWith (options^.surface) (options^.readable) bounds r
--       -- where bounds = sizeSpecToBounds (options^.sizeSpec)
--   
  adjustDia =
      adjustDiaSize2D (view sizeSpec) (set sizeSpec)

sizeSpec :: Lens' (Options PGFSys R2) SizeSpec2D
sizeSpec = lens getSize setSize
  where getSize (PGFOptions { _sizeSpec = s }) = s
        setSize o s = o { _sizeSpec = s }

style :: Style R2 -> P.Put
style s = do
  P.fillColor <*~ getFillColor
  P.fillRule  <~ getFillRule
  --
  P.lineColor <*~ getLineColor
  P.join <~ getLineJoin
  P.cap  <~ getLineCap
  P.dash <~ getDashing
  P.lw   <~ getLineWidth
  where
  (<~) :: (AttributeClass a) => (b -> P.Put) -> (a -> b) -> P.Put
  setter <~ getter = maybe (return ()) setter mAttribute
    where mAttribute = (getter <$>) . getAttr $ s
  --
  (<*~) :: (AttributeClass a, Color c)
        => (AlphaColour Double -> P.Put) -> (a -> c) -> P.Put
  setColor <*~ getColor = (setColor . fade . toAlphaColour) <~ getColor
  fade    = dissolve opacity
  opacity = maybe 1 getOpacity (getAttr s)

shouldStroke :: Style R2 -> Bool
shouldStroke s = maybe True (> P.epsilon) mLineWidth
  where
    mLineWidth = (getLineWidth <$>) . getAttr $ s

shouldFill :: Style R2 -> Bool
shouldFill s = isJust mFillColor
  where
    mFillColor = (getFillColor <$>) . getAttr $ s

draw :: Style R2 -> P.Put
draw s = do
  when (shouldFill s)   P.fill
  when (shouldStroke s) P.stroke


-- data RenderState = RenderState
--   { _ignoreFill :: Bool }
-- 
-- makeLenses ''RenderState

instance Monoid (Render PGFSys R2) where
  mempty                   = P $ return ()
  (P r1) `mappend` (P r2_) = P (r1 >> r2_)
------------------------------------------------------------------------
-- Renderable instances

instance Renderable (Segment Closed R2) PGFSys where
    render c = render c . (fromSegments :: [Segment Closed R2] -> Path R2) . (:[])

instance Renderable (Trail R2) PGFSys where
  render b = render b . pathFromTrail

instance Renderable (Path R2) PGFSys where
  render _ p = P $ do
    -- dirty hack, but we avoid needing state and how often to people try to 
    -- fill lines?
    when (any (isLine . unLoc) . op Path $ p) $ P.fillOpacity 0
    P.path p

