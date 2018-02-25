{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.PGF
-- Copyright   :  (c) 2015 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Interface to PGF. See the manual http://www.ctan.org/pkg/pgf for details.
--
------------------------------------------------------------------------------
module Graphics.Rendering.PGF
  ( renderWith
  , RenderM
  , Render
  , initialState
  -- * Environments
  , scope
  -- , scopeHeader
  -- , resetState
  -- , scopeFooter
  , epsilon
  -- * Lenses
  -- , fillRule
  , attributes
  -- * units
  , bp
  , pt
  , mm
  , px
  -- * RenderM commands
  , ln
  , raw
  , rawString
  , pgf
  , bracers
  , brackets
  -- * Paths
  -- , path
  -- , trail
  , segment
  , usePath
  , lineTo
  , curveTo
  , moveTo
  , closePath
  , clip
  , stroke
  , fill
  , asBoundingBox
  , path
  -- , rectangleBoundingBox
  -- * Strokeing Options
  , setDash
  , setLineWidth
  , setLineCap
  , setLineJoin
  , setMiterLimit
  , setLineColor
  , setLineOpacity
  -- * Fill Options
  , setFillColor
  , setFillRule
  , setFillOpacity
  -- * Transformations
  , setTransform
  , applyTransform
  , baseTransform
  , applyScale
  , resetNonTranslations
  -- * Shading
  -- , linearGradient
  -- , radialGradient
  , colorSpec
  , shadePath
  , opacityGroup
  -- * images
  , image
  , embeddedImage
  , embeddedImage'
  -- * Text
  , renderText
  , setTextAlign
  , setTextRotation
  , setFontWeight
  , setFontSlant
  ) where

import           Codec.Compression.Zlib
import           Codec.Picture
import           Control.Monad.RWS hiding ((<>))
import           Data.ByteString.Builder
import           Data.ByteString.Char8        (ByteString)
import qualified Data.ByteString.Char8        as B (replicate)
import           Data.ByteString.Internal     (fromForeignPtr)
import qualified Data.ByteString.Lazy         as LB
import qualified Data.Foldable                as F (foldMap)
import           Data.List                    (intersperse)
import           Data.Maybe                   (catMaybes)
-- import           Data.Typeable
import qualified Data.Vector.Storable         as S
-- import Control.Lens

-- import           Diagrams.Core.Transform
-- import           Diagrams.Prelude             hiding (Render, image, moveTo,
--                                                opacity, opacityGroup, stroke,
--                                                (<>))
import           Diagrams.TwoD.Text           (FontSlant (..), FontWeight (..),
                                               TextAlignment (..))

import Diagrams.Prelude hiding (moveTo, clip, stroke)
-- import Diagrams.Types.Style hiding (style)
-- import Diagrams.TwoD.Attributes hiding (clip)
import Diagrams.TwoD.Image hiding (image)
-- import Diagrams.TwoD.Path hiding (stroke)
-- import Geometry.Path.Unboxed
-- import Geometry.Trail.Unboxed

-- import Geometry.TwoD.Path
-- import Geometry.TwoD.Size
-- import Diagrams.TwoD.Image
-- import Geometry.Trail
-- import Geometry.Trail.Unboxed
-- import Geometry.Segment
-- import Geometry.Path
-- import Geometry.Path.Unboxed
-- import Geometry.TwoD.Types hiding (p2)
-- import Geometry.Transform hiding (moveTo)
-- import Data.Colour

-- import Diagrams.Attributes
-- import Diagrams.TwoD.Attributes
-- import Diagrams.Types
-- import Diagrams.Style

-- import Geometry.Points

import           Diagrams.Backend.PGF.Surface

import Data.Double.Conversion.ByteString as DC (toFixed)

-- * Types, lenses & runners

-- | Render state, mainly to be used for convenience when build, this module
--   only uses the indent properly.
data RenderState n = RenderState
  { _pos        :: Point V2 n -- ^ Current position
  , _indent     :: Int  -- ^ Current indentation
  }

makeLenses ''RenderState

data RenderInfo = RenderInfo
  { _format :: TexFormat
  , _pprint :: Bool
  , _attributes :: Attributes
  }

makeLenses ''RenderInfo

-- | Type for render monad.
type RenderM n m = RWS RenderInfo Builder (RenderState n) m

-- | Convenient type for building.
type Render n = RenderM n ()

instance Semigroup (Render n) where
  (<>) = (>>)
  {-# INLINE (<>) #-}

instance Monoid (Render n) where
  mempty = return ()
  {-# INLINE mempty #-}
  mappend = (>>)
  {-# INLINE mappend #-}

-- | Starting state for running the builder.
initialState :: (Floating n) => RenderState n
initialState = RenderState
  { _pos        = origin
  , _indent     = 0
  -- , _attributes = mempty -- Until I think of something better:
  -- , _attributes = lc black mempty -- Until I think of something better:
                                  -- (square 1 # opacity 0.5) doesn't work otherwise
  }

renderWith :: (RealFloat n)
  => Surface -> Bool -> Bool -> V2 n -> Render n -> Builder
renderWith s readable standalone bounds r = builder
  where
    bounds' = fmap (fromInteger . floor) bounds
    info    = RenderInfo (s^.texFormat) readable mempty
    builder = snd $ evalRWS r' info initialState
    r' = do
      when standalone $ do
        ln . rawString $ s^.preamble
        let pageSize = runPageSizeTemplate (ceiling <$> bounds) (s^.pageSizeTemplate)
        unless (null pageSize) $
          ln (rawString pageSize)
        ln . rawString $ s^.beginDoc
      picture $ rectangleBoundingBox bounds' >> r
      when standalone $ rawString $ s^.endDoc

-- low level utilities -------------------------------------------------

-- builder functions
raw :: Builder -> Render n
raw = tell
{-# INLINE raw #-}

rawByteString :: ByteString -> Render n
rawByteString = tell . byteString
{-# INLINE rawByteString #-}

rawString :: String -> Render n
rawString = tell . stringUtf8
{-# INLINE rawString #-}

pgf :: Builder -> Render n
pgf c = raw $ "\\pgf" <> c
{-# INLINE pgf #-}

rawChar :: Char -> Render n
rawChar = tell . char8
{-# INLINE rawChar #-}

-- | Emit the indentation when 'pprint' is True.
emit :: Render n
emit = do
  pp <- view pprint
  when pp $ do
    tab <- use indent
    rawByteString $ B.replicate tab ' '
{-# INLINE emit #-}

ln :: Render n -> Render n
ln r = do
  emit
  r
  rawChar '\n'
{-# INLINE ln #-}

-- | Wrap a `Render n` in { .. }.
bracers :: Render n -> Render n
bracers r = do
  rawChar '{'
  r
  rawChar '}'
{-# INLINE bracers #-}

bracersBlock :: Render n -> Render n
bracersBlock rs = do
  raw "{\n"
  inBlock rs
  emit
  rawChar '}'

-- | Wrap a `Render n` in [ .. ].
brackets :: Render n -> Render n
brackets r = do
  rawChar '['
  r
  rawChar ']'
{-# INLINE brackets #-}

parens :: Render n -> Render n
parens r = do
  rawChar '('
  r
  rawChar ')'
{-# INLINE parens #-}

-- | Intersperse list of Render ns with commas.
commaIntersperce :: [Render n] -> Render n
commaIntersperce = sequence_ . intersperse (rawChar ',')

-- | Place a Render n in an indented block
inBlock :: Render n -> Render n
inBlock r = do
  indent += 2
  r
  indent -= 2

-- numbers and points --------------------------------------------------

-- | Render a point.
point :: RealFloat n => P2 n -> Render a
point = tuplePoint . unp2

bracerPoint :: RealFloat n => P2 n -> Render a
bracerPoint (P (V2 x y)) = do
  bracers (bp x)
  bracers (bp y)

-- | Render n a tuple as a point.
tuplePoint :: RealFloat n => (n,n) -> Render a
tuplePoint (x,y) = do
  pgf "qpoint"
  bracers (bp x)
  bracers (bp y)

-- | Render n a n to four decimal places.
n :: RealFloat a => a -> Render n
-- n x = rawString $ showFFloat (Just 4) x ""
n x = rawByteString $ toFixed 4 (realToFrac x)

-- | Render n length with bp (big point = 1 px at 72 dpi) units.
bp :: RealFloat a => a -> Render n
bp = (>> raw "bp") . n

-- | Render n length with px units.
px :: RealFloat a => a -> Render n
px = (>> raw "px") . n

-- | Render n length with mm units.
mm :: RealFloat a => a -> Render n
mm = (>> raw "mm") . n -- . (*0.35278)

-- | Render n length with pt units.
pt :: RealFloat a => a -> Render n
pt = (>> raw "pt") . n -- . (*1.00375)

-- | ε = 0.0001 is the limit at which lines are no longer stroked.
epsilon :: Fractional n => n
epsilon = 0.0001

-- environments --------------------------------------------------------

picture :: Render n -> Render n
picture r = do
  f <- view format
  ln . raw $ case f of
    LaTeX    -> "\\begin{pgfpicture}"
    ConTeXt  -> "\\startpgfpicture"
    PlainTeX -> "\\pgfpicture"
  inBlock r
  ln . raw $ case f of
    LaTeX    -> "\\end{pgfpicture}"
    ConTeXt  -> "\\stoppgfpicture"
    PlainTeX -> "\\endpgfpicture"

rectangleBoundingBox :: RealFloat n => V2 n -> Render n
rectangleBoundingBox bounds = do
  ln $ do
    pgf "pathrectangle"
    bracers $ pgf "pointorigin"
    bracers $ tuplePoint (unr2 bounds)
  ln $ do
    pgf "usepath"
    bracers $ raw "use as bounding box"

-- | Wrap the rendering in a scope.
scope :: Render n -> Render n
scope r = do
  f <- view format
  ln . raw $ case f of
    LaTeX    -> "\\begin{pgfscope}"
    ConTeXt  -> "\\startpgfscope"
    PlainTeX -> "\\pgfscope"
  inBlock r
  ln . raw $ case f of
    LaTeX    -> "\\end{pgfscope}"
    ConTeXt  -> "\\stoppgfscope"
    PlainTeX -> "\\endpgfscope"

-- opacity groups ------------------------------------------------------

transparencyGroup :: Render n -> Render n
transparencyGroup r = do
  f <- view format
  ln . raw $ case f of
    LaTeX    -> "\\begin{pgftransparencygroup}"
    ConTeXt  -> "\\startpgftransparencygroup"
    PlainTeX -> "\\pgftransparencygroup"
  inBlock r
  ln . raw $ case f of
    LaTeX    -> "\\end{pgftransparencygroup}"
    ConTeXt  -> "\\stoppgftransparencygroup"
    PlainTeX -> "\\endpgftransparencygroup"

opacityGroup :: RealFloat a => a -> Render n -> Render n
opacityGroup x r = scope $ do
  setFillOpacity x
  transparencyGroup r

-- colours -------------------------------------------------------------

texColor :: RealFloat a => a -> a -> a -> Render n
texColor r g b = do
  n r
  rawChar ','
  n g
  rawChar ','
  n b

contextColor :: RealFloat a => a -> a -> a -> Render n
contextColor r g b = do
  raw "r=" >> n r
  rawChar ','
  raw "g=" >> n g
  rawChar ','
  raw "b=" >> n b

-- | Defines an RGB colour with the given name, using the Tex format.
defineColour :: RealFloat a => ByteString -> a -> a -> a -> Render n
defineColour name r g b = do
  f <- view format
  ln $ case f of
    ConTeXt  -> do
      raw "\\definecolor"
      brackets $ rawByteString name
      brackets $ contextColor r g b
    _        -> do
      raw "\\definecolor"
      bracers $ rawByteString name
      bracers $ raw "rgb"
      bracers $ texColor r g b

parensColor :: Color c => c -> Render n
parensColor c = parens $ texColor r g b
  where (r,g,b,_) = colorToSRGBA c


-- paths ---------------------------------------------------------------

-- | Close the current path.
closePath :: Render n
closePath = ln $ pgf "pathclose"

-- | Move path to point.
moveTo :: RealFloat n => P2 n -> Render n
moveTo v = ln $ do
  pos .= v
  pgf "pathqmoveto"
  bracerPoint v

-- | Move path by vector.
lineTo :: RealFloat n => V2 n -> Render n
lineTo v = ln $ do
  p <- use pos
  let v' = p .+^ v
  pos .= v'
  pgf "pathqlineto"
  bracerPoint v'

-- | Make curved path from vectors.
curveTo :: RealFloat n => V2 n -> V2 n -> V2 n -> Render n
curveTo v2 v3 v4 = ln $ do
  p <- use pos
  let [v2',v3',v4'] = map (p .+^) [v2,v3,v4]
  pos .= v4'
  pgf "pathqcurveto"
  mapM_ bracerPoint [v2', v3', v4']

-- | Move path by vector.
lineTo' :: RealFloat n => P2 n -> Render n
lineTo' p = ln $ do
  pgf "pathqlineto"
  bracerPoint p

-- | Make curved path from vectors.
curveTo' :: RealFloat n => P2 n -> P2 n -> P2 n -> Render n
curveTo' p2 p3 p4 = ln $ do
  pgf "pathqcurveto"
  mapM_ bracerPoint [p2, p3, p4]


-- | Stroke the defined path using parameters from current scope.
stroke :: Render n
stroke = ln $ pgf "usepathqstroke"

-- | Fill the defined path using parameters from current scope.
fill :: Render n
fill = ln $ pgf "usepathqfill"

-- | Use the defined path a clip for everything that follows in the current
--   scope. Stacks.
clip :: Render n
clip = ln $ pgf "usepathqclip"

-- path :: RealFloat n => Path V2 n -> Render n
-- path (Path trs) = do
--   mapM_ renderTrail trs
--   where
--     renderTrail (viewLoc -> (p, tr)) = do
--       moveTo p
--       trail tr

-- trail :: RealFloat n => Trail V2 n -> Render n
-- trail t = withLine (render' . lineSegments) t
--   where
--     render' segs = do
--       mapM_ segment segs
--       when (isLoop t) closePath

segment :: RealFloat n => Segment V2 n -> Render n
segment (Linear v)       = lineTo v
segment (Cubic v1 v2 v3) = curveTo v1 v2 v3

path :: RealFloat n => T2 n -> Path V2 n -> Render n
path t = foldMapOf each (trail . transform t)

trail :: RealFloat n => Located (Trail V2 n) -> Render n
trail (Loc p0 t) = do
  moveTo p0
  case t of
    ClosedTrail (Loop l c) -> do
      line l
      p <- use pos
      close p p0 c
    OpenTrail l            -> line l

line :: RealFloat n => Line V2 n -> Render n
line = traverseOf_ segments segment

-- trail :: (Unboxable V2 n, RealFloat n) => T2 n -> Located (UTrail V2 n) -> Render n
-- trail t2 (Loc (papply t2 -> p) t) = case t of
--   UTrail (ULine (SegVector v))   -> snd $ uVector t2 p v
--   UTrail (ULoop (SegVector v) o) ->
--     let (p', r) = uVector t2 p v
--     in  r >> close p' p (transform t2 o)

close :: RealFloat n =>  P2 n -> P2 n -> ClosingSegment V2 n -> Render n
close pStart pEnd = \case
  LinearClosing      -> closePath
  CubicClosing v1 v2 -> curveTo' (pStart .+^ v1) (pStart .+^ v2) pEnd >> closePath

-- uVector :: (Unboxable V2 n, RealFloat n)
--         => T2 n -> P2 n -> Vector (ClosedSegment V2 n) -> (P2 n, Render n)
-- uVector t2 p v = foldSegmentsT lineTo' curveTo' t2 p v (moveTo p)

-- | @usePath fill stroke@ combined in one function.
usePath :: Bool -> Bool -> Render n
usePath False False     = return ()
usePath doFill doStroke = ln $ do
  pgf "usepathq"
  when doFill $ raw "fill"
  when doStroke $ raw "stroke"

-- | Uses the current path as the bounding box for whole picture.
asBoundingBox :: Render n
asBoundingBox = ln $ do
  pgf "usepath"
  bracers $ raw "use as bounding box"

-- rectangleBoundingBox :: (n,n) -> Render n
-- rectangleBoundingBox xy = do
--   ln $ do
--     pgf "pathrectangle"
--     bracers $ pgf "pointorigin"
--     bracers $ tuplePoint xy
--   asBoundingBox

-- stroke properties

-- | Sets the line width in current scope. Must be done before stroking.
setLineWidth :: RealFloat n => n -> Render n
setLineWidth w = ln $ do
  pgf "setlinewidth"
  bracers $ bp w

-- | Sets the line cap in current scope. Must be done before stroking.
setLineCap :: LineCap -> Render n
setLineCap cap = ln . pgf $ case cap of
   LineCapButt   -> "setbuttcap"
   LineCapRound  -> "setroundcap"
   LineCapSquare -> "setrectcap"

-- | Sets the line join in current scope. Must be done before stroking.
setLineJoin :: LineJoin -> Render n
setLineJoin lJoin = ln . pgf $ case lJoin of
   LineJoinBevel -> "setbeveljoin"
   LineJoinRound -> "setroundjoin"
   LineJoinMiter -> "setmiterjoin"

-- | Sets the miter limit in the current scope. Must be done before stroking.
setMiterLimit :: RealFloat n => n -> Render n
setMiterLimit l = do
  pgf "setmiterlimit"
  bracers $ bp l

-- stroke parameters ---------------------------------------------------

-- | Sets the dash for the current scope. Must be done before stroking.
setDash :: RealFloat n => Dashing n -> Render n
setDash (Dashing ds offs) = setDash' ds offs

-- \pgfsetdash{{0.5cm}{0.5cm}{0.1cm}{0.2cm}}{0cm}
-- | Takes the dash distances and offset, must be done before stroking.
setDash' :: RealFloat n => [n] -> n -> Render n
setDash' ds off = ln $ do
  pgf "setdash"
  bracers $ mapM_ (bracers . bp) ds
  bracers $ bp off

-- | Sets the stroke colour in current scope. If colour has opacity < 1, the
--   scope opacity is set accordingly. Must be done before stroking.
setLineColor :: (RealFloat a, Color c) => c -> Render a
setLineColor c = do
  defineColour "sc" r g b
  ln $ pgf "setstrokecolor{sc}"
  --
  when (a /= 1) $ setLineOpacity (realToFrac a)
  where
    (r,g,b,a) = colorToSRGBA c

-- | Sets the stroke opacity for the current scope. Should be a value between 0
--   and 1. Must be done  before stroking.
setLineOpacity :: RealFloat n => n -> Render n
setLineOpacity a = ln $ do
  pgf "setstrokeopacity"
  bracers $ n a

-- filling -------------------------------------------------------------

-- | Set the fill rule to winding or even-odd for current scope. Must be done
--   before filling.
setFillRule :: FillRule -> Render n
setFillRule rule = ln $ case rule of
  Winding -> pgf "setnonzerorule"
  EvenOdd -> pgf "seteorule"

-- | Sets the fill colour for current scope. If an alpha colour is used, the
--   fill opacity is set accordingly. Must be done before filling.
setFillColor :: Color c => c -> Render n
setFillColor (colorToSRGBA -> (r,g,b,a)) = do
  defineColour "fc" r g b
  ln $ pgf "setfillcolor{fc}"
  --
  when (a /= 1) $ setFillOpacity (realToFrac a :: Double)

-- | Sets the stroke opacity for the current scope. Should be a value between 0
--   and 1. Must be done  before stroking.
setFillOpacity :: RealFloat a => a -> Render n
setFillOpacity a = ln $ do
  pgf "setfillopacity"
  bracers $ n a

-- transformations -----------------------------------------------------

getMatrix :: Num n => Transformation V2 n -> (n, n, n, n, n, n)
getMatrix t = (a1,a2,b1,b2,c1,c2)
 where
   [a1, a2, b1, b2, c1, c2] = concat $ matrixHomRep t

-- \pgftransformcm{⟨a⟩}{⟨b⟩}{⟨c⟩}{⟨d⟩}{⟨pointa}

-- | Applies a transformation to the current scope. This transformation only
--   effects coordinates and text, not line withs or dash spacing. (See
--   applyDeepTransform). Must be set before the path is used.
applyTransform :: RealFloat n => Transformation V2 n -> Render n
applyTransform t
  | isID      = return ()
  | shiftOnly = ln $ do
      pgf "transformshift"
      bracers p
  | otherwise = ln $ do
    pgf "transformcm"
    mapM_ (bracers . n) [a, b, c, d] >> bracers p
  where
    (a,b,c,d,e,f) = getMatrix t
    p             = tuplePoint (e,f)
    --
    shiftOnly = (a,b,c,d) == (1,0,0,1)
    isID      = shiftOnly && (e,f) == (0,0)

-- | Resets the transform and sets it. Must be set before the path is used.
setTransform :: RealFloat n => Transformation V2 n -> Render n
setTransform t = do
  pgf "settransformentries"
  mapM_ (bracers . n) [a, b, c, d] >> mapM_ (bracers . bp) [e, f]
  where
    (a,b,c,d,e,f) = getMatrix t

applyScale :: RealFloat n => n -> Render n
applyScale s = ln $ do
  pgf "transformscale"
  bracers $ n s

resetNonTranslations :: Render n
resetNonTranslations = ln $ pgf "transformresetnontranslations"

-- | Base transforms are applied by the document reader.
baseTransform :: RealFloat n => Transformation V2 n -> Render n
baseTransform t = ln $ do
  pgf "lowlevel"
  bracers $ setTransform t

-- setShadetransform :: Transformation V2 -> Render n
-- setShadetransform (dropTransl -> t) = do
--   pgf "setadditionalshadetransform"
--   bracersBlock $ applyTransform t

-- shading -------------------------------------------------------------

-- linearGradient :: Path V2 Double -> LGradient -> Render Double
-- linearGradient p lg = scope $ do
--   path p
--   let (stops', t) = calcLinearStops (getEnvelope p) lg
--   ln $ do
--     pgf "declarehorizontalshading"
--     bracers $ raw "ft"    -- fill texture
--     bracers $ raw "100bp" -- gradient is always 100 x 100 square
--     bracersBlock $ colorSpec 1 stops'
--   clip
--   baseTransform t
--   useShading $ raw "ft"

-- | Calculate the correct linear stops such that the path is completely
--   filled. PGF doesn't have spread methods so this has to be done
--   manually.
calcLinearStops
  :: Envelope V2 Double  -- ^ envelope of object being covered
  -> LGradient
  -> ([GradientStop], T2 Double)
calcLinearStops EmptyEnvelope _ = ([], mempty)
calcLinearStops _ _ = undefined
  -- = (linearStops' x0 x1 stops sm, t <> ft)
  -- where
  --   -- Transform such that the transform t origin is start of the
  --   -- gradient, transform t unitX is the end.
  --   t = gt
  --       -- encorperate the start and end points
  --    -- <> translation (p0 ^. _Point)
  --    -- <> scaling (norm (p1 .-. p0))
  --    -- <> rotationTo (dirBetween p1 p0)

  --   -- Use the inverse transformed path and make the pre-transformed
  --   -- gradient fit to it. Then when we transform the gradient we know
  --   -- it'll fit the path.
  --   env'         = transform (inv t) env
  --   Just (x0,x1) = extentX env'
  --   Just (y0,y1) = extentY env'

  --   -- Final transform to fit the gradient to the path. The origin on
  --   -- the gradient is its centre so we translate by - V2 50 50 to get
  --   -- to the lower corner (because of this we set the size of the
  --   -- gradient to always be 100 x 100 for simplicity). Then scales up
  --   -- the gradient to cover the path and moves it into position.
  --   ft = translation (V2 x0 y0) <> scalingV ((*0.01) . abs <$> V2 (x0 - x1) (y0 - y1)) <> translation 50

useShading :: Render n -> Render n
useShading nm = ln $ do
  pgf "useshading"
  bracers nm

-- _translation :: Lens' (Transformation v n) (v n)
-- _translation f (Transformation a b v) = f v <&> \v' -> Transformation a b v'

linearStops' :: Double -> Double -> [GradientStop] -> SpreadMethod -> [GradientStop]
linearStops' x0 x1 stops sm =
  GradientStop c1' 0 : filter (inRange . view stopFraction) stops' ++ [GradientStop c2' 100]
  where
    stops' = case sm of
      GradPad     -> over (each . stopFraction) normalise stops
      GradRepeat  -> flip F.foldMap [i0 .. i1] $ \i ->
                   increaseFirst $
                     over (each . stopFraction)
                          (normalise . (+ fromIntegral i))
                          stops
      GradReflect -> flip F.foldMap [i0 .. i1] $ \i ->
                   over (each . stopFraction)
                        (normalise . (+ fromIntegral i))
                        (reverseOdd i stops)

    -- for repeat it sometimes complains if two are exactly the same so
    -- increase the first by a little
    increaseFirst = over (_head . stopFraction) (+0.001)
    reverseOdd i
      | odd i     = reverse . over (each . stopFraction) (1 -)
      | otherwise = id
    i0 = floor x0 :: Int
    i1 = ceiling x1
    c1' = toAlphaColour $ colourInterp stops' 0
    c2' = toAlphaColour $ colourInterp stops' 100
    inRange x   = x > 0 && x < 100
    normalise x = 100 * (x - x0) / (x1 - x0)

colourInterp :: [GradientStop] -> Double -> AlphaColour Double
colourInterp cs0 x = go cs0
  where
    go (GradientStop c1 a : c@(GradientStop c2 b) : cs)
      | x <= a         = toAlphaColour c1
      | x > a && x < b = blend y (toAlphaColour c2) (toAlphaColour c1)
      | otherwise      = go (c : cs)
      where
        y = realToFrac $ (x - a) / (b - a)
    go [GradientStop c2 _] = toAlphaColour c2
    go _ = transparent

-- radialGradient :: Path V2 Double -> RGradient -> Render Double
-- radialGradient p rg = scope $ do
--   path p
--   let (stops', t, p0) = calcRadialStops (getEnvelope p) rg
--   ln $ do
--     pgf "declareradialshading"
--     bracers $ raw "ft"
--     bracers $ point p0
--     bracersBlock $ colorSpec 1 stops'
--   clip
--   baseTransform t
--   useShading $ raw "ft"

-- | Calculate the correct linear stops such that the path is completely
--   filled. PGF doesn't have spread methods so this has to be done
--   manually.
calcRadialStops :: Envelope V2 Double -> RGradient -> ([GradientStop], T2 Double, P2 Double)
calcRadialStops EmptyEnvelope _ = ([], mempty, origin)
calcRadialStops env _ = undefined -- (RGradient stops p0 r0 p1 r1 gt _sm)
  -- = (stops', t <> ft, P cv)
  -- where
  --   cv = tp0 .-. tp1
  --   tp0 = papply gt p0
  --   tp1 = papply gt p1
  --   -- Transform such that the transform t origin is start of the
  --   -- gradient, transform t unitX is the end.
  --   t = gt
  --    <> translation (p1 ^. _Point)
  --    <> scaling r1

  --   -- Similar to linear gradients but not so precise, d is a (bad and
  --   -- probably incorrect) lower bound for the required radius of the
  --   -- circle to cover the path.
  --   env'         = transform (inv t) env
  --   Just (x0,x1) = extentX env'
  --   Just (y0,y1) = extentY env'
  --   d = 2 * max (max (abs $ x0 - x1) (abs $ y0 - y1)) (lstop ^. stopFraction)

  --   -- Adjust for gradient size having radius 100
  --   ft = scaling 0.01

  --   -- Stops are scaled to start at r0 and end at r1. The gradient is
  --   -- extended to d to try to cover the path.
  --   --
  --   -- The problem is extending the size of the gradient in this way
  --   -- affects how the gradient scales if it is off-centre. This needs
  --   -- to be fixed.
  --   --
  --   -- Only the Pad spread method is supported for now.
  --   stops' = head stops : over (each . stopFraction) refrac stops ++ [lstop & stopFraction .~ 100*d]
  --   refrac x = 100 * ((r0 + x * (r1 - r0)) / r1) -- start at r0, end at r1
  --   lstop = last stops

-- Dirty adjustments for spread methods (PGF doesn't seem to have them).
-- adjustStops :: RealFloat n => [GradientStop n] -> SpreadMethod -> [GradientStop n]
-- adjustStops stops method =
--   case method of
--     Pad     -> (stopFraction .~ 0) (head stops) : map (stopFraction +~ 1) stops
--             ++ [(stopFraction +~ 2) (last stops)]
--     Reflect -> correct . concat . replicate 10
--              $ [stops, zipWith (\a b -> a & (stopColor .§ b)) stops (reverse stops)]
--     Repeat  -> correct . replicate 10 $ stops
  -- where
  --   correct  = ifoldMap (\i -> map (stopFraction +~ (lastStop * fromIntegral i)) )
  --   lastStop = last stops ^. stopFraction

-- (.§) :: Lens s t b b -> s -> s -> t
-- (.§) l a b = b & l #~ (a ^# l)
-- {-# INLINE (.§) #-}

colorSpec :: Double -> [GradientStop] -> Render Double
colorSpec d = mapM_ ln
            . combinePairs
            . intersperse (rawChar ';')
            . map mkColor
  where
    mkColor (GradientStop c sf) = do
      raw "rgb"
      parens $ bp (d*sf)
      raw "="
      parensColor c

combinePairs :: Monad m => [m a] -> [m a]
combinePairs (x1:x2:xs) = (x1 >> x2) : combinePairs xs
combinePairs xs         = xs

shadePath :: RealFloat n => Angle n -> Render n -> Render n
shadePath (view deg -> θ) name = ln $ do
  pgf "shadepath"
  bracers name
  bracers $ n θ

-- external images -----------------------------------------------------

-- \pgfimage[⟨options ⟩]{⟨filename ⟩}

-- | Images are wraped in a \pgftext.
image :: RealFloat n => T2 n -> DImage n External -> Render n
image t2 (DImage w h (ImageRef ref)) = scope $ do
  applyTransform t2
  ln $ do
    pgf "text"
    bracers $ do
      pgf "image"
      brackets $ do
        raw "width=" >> bp (fromIntegral w :: Double)
        rawChar ','
        raw "height=" >> bp (fromIntegral h :: Double)
      bracers $ rawString ref

-- embedded images -----------------------------------------------------

embeddedImage :: RealFloat n => T2 n -> DImage n Embedded -> Render n
embeddedImage t (DImage w h (ImageRaster (ImageRGB8 img))) =
  embeddedImage' (hexImage img) w h t
  -- TODO: Support more formats (like grey scale and alpha channels)
embeddedImage _ _ = error "Unsupported embedded image. Only ImageRGB8 is currently supported."

-- | Convert an 'Image' to a zlib compressed lazy 'ByteString' of the
--   raw image data. This is a suitable format for an embedded PDF image
--   stream.
hexImage :: Image PixelRGB8 -> LB.ByteString
hexImage (imageData -> v) = compress $ LB.fromStrict bs
  where
    bs         = fromForeignPtr p i nn
    (p, i, nn) = S.unsafeToForeignPtr v

embeddedImage' :: RealFloat n => LB.ByteString -> Int -> Int -> T2 n -> Render n
embeddedImage' img w h t = scope $ do
  baseTransform t
  ln $ raw "\\immediate\\pdfliteral{"
  rawLn "q" -- save state

  -- Scale the image to it's actual size and translate so the origin is
  -- at the centre.
  rawLn $ s w <> " 0 0 " <> s h <> " -" <> half w <> " -" <> half h <> " cm"
  rawLn "BI"           -- begin image
  rawLn $ "/W " <> s w -- width in pixels
  rawLn $ "/H " <> s h -- height in pixels
  rawLn "/CS /RGB"     -- RGB colour space
  rawLn "/BPC 8"       -- 8 bits per component

  -- Filters for the encoded image:
  --   ASCIIHexDecode -- decode from hexadecimal to binary
  --   FlateDecode    -- decompress using zlib deflate compression
  rawLn "/F [/AHx /Fl]"

  -- We use hex format for the image data so tex can output it without
  -- any problems. Base85 might be possible and would be 2-3x smaller
  -- but there's some problem chars tex complains about. Base64 would
  -- be ideal but the pdf spec doesn't seem to support it.
  --
  -- This is an inline image which is only really suitable for small
  -- images. An XObject might be more appropriate. See
  -- http://partners.adobe.com/public/developer/en/pdf/PDFReference.pdf
  -- for more information.
  rawLn "ID" -- image data
  rawLn $ hexChunk img <> char8 '>'
  rawLn "EI" -- end image
  rawLn "Q"  -- restore state
  rawLn "}"
    where
      rawLn r = raw r >> rawChar '\n'
      s       = intDec
      half x  = s (x `div` 2) <> if odd x then ".5" else mempty

-- | Insert hex encode and add a newline every 80 chars. This is useful for
--   readable output and stopping tex from choking when streaming. Note
--   that new-lines and spaces are ignored with the hex decode filter.
hexChunk :: LB.ByteString -> Builder
hexChunk (LB.splitAt 40 -> (a,b))
  | LB.null b = lazyByteStringHex a
  | otherwise = lazyByteStringHex a <> char8 '\n' <> hexChunk b

-- text ----------------------------------------------------------------

renderText :: [Render n] -> Render n -> Render n
renderText ops txt = ln $ do
  pgf "text"
  brackets . commaIntersperce $ ops
  bracers txt

-- | Returns a list of values to be put in square brackets like
--   @\pgftext[left,top]{txt}@.
setTextAlign :: RealFloat n => TextAlignment n -> [Render n]
setTextAlign a = case a of
  BaselineText         -> [raw "base", raw "left"]
  BoxAlignedText xt yt -> catMaybes [xt', yt']
    where
      xt' | xt > 0.75 = Just $ raw "right"
          | xt < 0.25 = Just $ raw "left"
          | otherwise = Nothing
      yt' | yt > 0.75 = Just $ raw "top"
          | yt < 0.25 = Just $ raw "bottom"
          | otherwise = Nothing

setTextRotation :: RealFloat n => Angle n -> [Render n]
setTextRotation a = case a^.deg of
  0 -> []
  θ -> [raw "rotate=" >> n θ]

-- | Set the font weight by rendering @\bf @. Nothing is done for normal
--   weight.
setFontWeight :: FontWeight -> Render n
setFontWeight FontWeightBold = raw "\\bf "
setFontWeight _              = return ()

-- | Set the font slant by rendering @\bf @. Nothing is done for normal weight.
setFontSlant :: FontSlant -> Render n
setFontSlant FontSlantNormal  = return ()
setFontSlant FontSlantItalic  = raw "\\it "
setFontSlant FontSlantOblique = raw "\\sl "
