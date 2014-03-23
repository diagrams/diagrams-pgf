{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Puting.PGFSystem
-- Maintainer  :  c.chalmers@me.com
--
-- Interface to the system layer of PGF. This is intented to be the rendering 
-- engine for standalone PGF diagrams because of it's difficuily when working 
-- with it. Currently incomplete.
--
------------------------------------------------------------------------------
module Graphics.Rendering.PGFSystem
  ( render
  , renderWith
  , PutM
  , Put
  , raw
  , rawString
  -- * Environments
  , picture
  , scope
  , epsilon
  -- * Paths
  , lineTo
  , curveTo
  , moveTo
  , closePath
  , clip
  , stroke
  , fill
  -- * Strokeing Options
  , dash
  , lw
  , lineCap
  , lineJoin
  , lineMiterLimit
  , lineColor
  , lineOpacity
  -- * Fill Options
  , fillColor
  , eoRule
  , fillRule
  , fillOpacity
  -- * Transformations
  , transform
  , scale
  , shift
  -- * images
  -- * Text
  , paperSize
    -- * Diagrams primitves
  , path
  , locTrail
  -- , trail
  , segment
  ) where

import Data.Monoid
import Control.Applicative
import Control.Monad (when, zipWithM_)
import Blaze.ByteString.Builder as Blaze
import Blaze.ByteString.Builder.Char.Utf8 as Blaze
import Data.Double.Conversion.ByteString
import Data.ByteString.Char8 (ByteString)
import Data.List (intersperse)
import Data.AdditiveGroup ((^+^))

import Diagrams.Core.Transform (Transformation, apply, transl)

import Diagrams.Attributes (LineCap(..), LineJoin(..), Dashing(..),
                            colorToSRGBA, Color (..))
import Diagrams.TwoD.Path (FillRule(..), Clip (..))
import Diagrams.TwoD.Vector (unitX, unitY)
import Diagrams.Trail (Trail, trailSegments, isLoop, trailVertices)
import Diagrams.Path (Path (..))
import Diagrams.Located (Located, viewLoc)
import Diagrams.Segment -- (Segment, OffsetClosed (..))
import Diagrams.TwoD.Types (R2, P2, unr2, unp2, r2)
-- import Diagrams.TwoD.Text
-- import Diagrams.TwoD.Image

import Diagrams.Backend.PGF.Surface
import Control.Lens ((^.))

data PairS a = PairS a !Builder

sndS :: PairS a -> Builder
sndS (PairS _ b) = b

-- | The PutM type. A Writer monad over the efficient Builder monoid.
newtype PutM a = Put { unPut :: PairS a }

-- | Put merely lifts Builder into a Writer monad, applied to ().
type Put = PutM ()

instance Functor PutM where
  fmap f m = Put $ let PairS a w = unPut m in PairS (f a) w
  {-# INLINE fmap #-}

instance Applicative PutM where
  pure    = return
  m <*> k = Put $
      let PairS f w  = unPut m
          PairS x w' = unPut k
      in PairS (f x) (w `mappend` w')

-- Standard Writer monad, with aggressive inlining
instance Monad PutM where
  return a = Put $ PairS a mempty
  {-# INLINE return #-}

  m >>= k  = Put $
      let PairS a w  = unPut m
          PairS b w' = unPut (k a)
      in PairS b (w `mappend` w')
  {-# INLINE (>>=) #-}

  m >> k  = Put $
      let PairS _ w  = unPut m
          PairS b w' = unPut k
      in PairS b (w `mappend` w')
  {-# INLINE (>>) #-}


render :: PutM a -> Builder
render = sndS . unPut

renderWith :: Surface -> Bool -> (Double,Double) -> Put -> Builder
renderWith s standalone bounds r = render $ do
      when standalone $ do
        ln . rawString $ s^.preamble
        maybe (paperSize bounds) (rawString . ($ bounds)) (s^.pageSize)
        ln . rawString $ s^.beginDoc
      picture r
      when standalone $ rawString $ s^.endDoc

-- builder functions

tell :: Blaze.Builder -> Put
tell b = Put $ PairS () b
{-# INLINE tell #-}

raw :: ByteString -> Put
raw = tell . Blaze.fromByteString

rawString :: String -> Put
rawString = tell . Blaze.fromString

sys :: ByteString -> Put
sys c = raw "\\pgfsys@" >> raw c

rawChar :: Char -> Put
rawChar = tell . Blaze.fromChar

ln :: Put -> Put
ln = (>> rawChar '\n')

-- | Wrap a `Put` in { .. }.
bracers :: Put -> Put
bracers r = do
  rawChar '{'
  r
  rawChar '}'

commaIntersperse :: [Put] -> Put
commaIntersperse = sequence_ . intersperse (rawChar ',')

-- * number and points

p :: R2 -> Put
p = p' . unr2

p' :: (Double,Double) -> Put
p' (x,y) = do
  bracers (px x)
  bracers (px y)

n :: Double -> Put
n = bracers . show4

cm :: Double -> Put
cm = (>> raw "cm") . show4

show4 :: Double -> Put
show4 = raw . toFixed 4

px :: Double -> Put
px = (>> raw "px") . show4

-- | ε = 0.0001 is the limit at which lines are no longer stroked.
epsilon :: Double
epsilon = 0.0001


-- * PGF environments

picture :: Put -> Put
picture r = do
  beginPicture
  r
  endPicture

beginPicture :: Put
beginPicture = ln $ sys "beginpicture"

endPicture :: Put
endPicture = ln $ sys "endpicture"

-- | Wrap the Puting in a scope.
scope :: Put -> Put
scope r = do
  beginScope
  r
  endScope

-- | Header for starting a scope.
beginScope :: Put
beginScope = ln $ sys "beginscope"

-- | Footer for ending a scope.
endScope :: Put
endScope = ln $ sys "endscope"

-- transformations

transform :: Transformation R2 -> Put
transform = transform' . getMatrix

getMatrix :: Transformation R2
          -> (Double, Double, Double, Double, Double, Double)
getMatrix t = (a1,a2,b1,b2,c1,c2)
 where
  (unr2 -> (a1,a2)) = apply t unitX
  (unr2 -> (b1,b2)) = apply t unitY
  (unr2 -> (c1,c2)) = transl t

transform' :: (Double,Double,Double,Double,Double,Double) -> Put
transform' (a,b,c,d,e,f) = ln $ do
  sys "transformcm"
  mapM_ n [a,b,c,d]
  p' (e,f)

shift :: R2 -> Put
shift v = ln $ do
  sys "transformshift"
  p v

scale :: (Double,Double) -> Put
scale s = ln $ do
  sys "transformxyscale"
  p' s


-- Path commands

moveTo :: R2 -> Put
moveTo v = ln $ do
  sys "moveto"
  p v

lineTo :: R2 -> Put
lineTo v = ln $ do
  sys "lineto"
  p v

curveTo :: R2 -> R2 -> R2 -> Put
curveTo v2 v3 v4 = ln $ do
  sys "curveto"
  mapM_ p [v2,v3,v4]

-- using paths

closePath :: Put
closePath = ln $ sys "closepath"

stroke :: Put
stroke = ln $ sys "stroke"

-- strokeClose :: Put
-- strokeClose = ln $ sys "closestroke"

fill :: Put
fill = ln $ sys "fill"

clip :: Clip -> Put
clip (Clip paths) = mapM_ clipPath paths

clipPath :: Path R2 -> Put
clipPath (Path locTrails) = do
  mapM_ locTrail locTrails
  clipNext

clipNext :: Put
clipNext = ln $ do
  sys "clipnext"
  sys "discardpath"
  -- this is the only way diagrams spcifies clips

lw :: Double -> Put 
lw w = ln $ do
  sys "setlinewidth"
  n w

-- properties

lineCap :: LineCap -> Put
lineCap c = ln . sys $ case c of
  LineCapButt   -> "buttcap"
  LineCapRound  -> "roundcap"
  LineCapSquare -> "rectcap"

lineJoin :: LineJoin -> Put
lineJoin j = ln . sys $ case j of
  LineJoinBevel -> "beveljoin"
  LineJoinRound -> "roundjoin"
  LineJoinMiter -> "miterjoin"

lineMiterLimit :: Double -> Put
lineMiterLimit l = ln $ do
  sys "setmiterlimit"
  n l

dash :: Dashing -> Put
dash (Dashing ds ph) = dash' ds ph

dash' :: [Double] -> Double -> Put
dash' ds ph = ln $ do
  bracers . commaIntersperse $ map cm ds
  bracers $ cm ph

eoRule :: Put
eoRule = raw "\\ifpgfsys@eorule" -- not sure about this

fillRule :: FillRule -> Put
fillRule EvenOdd = eoRule
fillRule _       = return ()

-- * Colours

lineColor :: (Color c) => c -> Put
lineColor c = ln $ do
  lineColor' r g b
  when (a /= 1) $ lineOpacity a
  where (r,g,b,a) = colorToSRGBA c

lineOpacity :: Double -> Put
lineOpacity o = ln $ do
  sys "stroke@opacity"
  n o

lineColor' :: Double -> Double -> Double -> Put
lineColor' r g b = ln $ do
  sys "color@rgb@stroke"
  mapM_ n [r,g,b]

fillColor :: (Color c) => c -> Put
fillColor c = ln $ do
  fillColor' r g b
  when (a /= 1) $ fillOpacity a
  where (r,g,b,a) = colorToSRGBA c

fillOpacity :: Double -> Put
fillOpacity o = ln $ do
  sys "fill@opacity"
  n o

fillColor' :: Double -> Double -> Double -> Put
fillColor' r g b = do
  sys "color@rgb@fill"
  mapM_ n [r,g,b]

paperSize :: (Double, Double) -> Put
paperSize s = ln $ do
  sys "papersize"
  p' s

-- * Diagram primitves

path :: Path R2 -> Put
path (Path trs) = mapM_ locTrail trs

-- locTrail :: Located (Trail R2) -> Put
-- locTrail (viewLoc -> (unp2 -> p2, t)) = do
--   moveTo (r2 p2)
--   trail t
-- 
-- trail :: Trail R2 -> Put
-- trail t@(trailSegments -> segs) = do
--   mapM_ segment segs
--   when (isLoop t) closePath

locTrail :: Located (Trail R2) -> Put
locTrail lT@(viewLoc -> (unp2 -> p2, t@(trailSegments -> segs))) = do
  moveTo (r2 p2)
  let verts = trailVertices lT
  zipWithM_ moveSeg segs verts
  when (isLoop t) closePath

moveSeg :: Segment Closed R2 -> P2 -> Put
moveSeg (Linear (OffsetClosed v)) p2       = lineTo (v ^+^ (r2 . unp2) p2)
moveSeg (Cubic v1 v2 (OffsetClosed v3)) p2 = curveTo v1' v2' v3'
  where [v1',v2',v3'] = map (^+^ (r2 . unp2) p2) [v1,v2,v3]

segment :: Segment Closed R2 -> Put
segment (Linear (OffsetClosed v))       = lineTo v
segment (Cubic v1 v2 (OffsetClosed v3)) = curveTo v1 v2 v3

