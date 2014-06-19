{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.PGF
-- Copyright   :  (c) 2014 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  c.chalmers@me.com
--
-- Interface to PGF. See the manual http://www.ctan.org/pkg/pgf for details.
--
------------------------------------------------------------------------------
module Graphics.Rendering.PGF
  ( renderWith
  , RenderM
  , Render
  , initialState
  , rawString
  -- * Environments
  , scope
  , scopeHeader
  , resetState
  , scopeFooter
  , epsilon
  -- * Lenses
  -- , fillRule
  , ignoreFill
  , style
  -- * units
  , bp
  , pt
  , mm
  , px
  -- * RenderM commands
  , ln
  , raw
  , pgf
  , bracers
  , brackets
  -- * Paths
  , usePath
  , lineTo
  , curveTo
  , moveTo
  , closePath
  , clip
  , stroke
  , fill
  , asBoundingBox
  -- , rectangleBoundingBox
  , applyOpacity
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
  , linearGradient
  , radialGradient
  , colorSpec
  , shadePath
  -- * images
  , image
  -- * Text
  , renderText
  , setTextAlign
  , setTextRotation
  , setFontWeight
  , setFontSlant
  ) where

import           Blaze.ByteString.Builder           as Blaze (Builder, fromByteString)
import           Blaze.ByteString.Builder.Char.Utf8 as Blaze (fromChar, fromString)
import           Control.Lens                       (Lens, both,
                                                     imap, makeLenses,
                                                     over, use, view,
                                                     ( #~ ), (+=), (+~),
                                                     (-=), (.=), (^#),
                                                     (^.))
import           Control.Monad.RWS
import           Data.ByteString.Char8              (ByteString)
import qualified Data.ByteString.Char8              as B (replicate)
import           Data.Double.Conversion.ByteString  (toFixed)
import           Data.List                          (intersperse)
import           Data.Maybe                         (catMaybes)

import Diagrams.Prelude    hiding (Render, image, moveTo, opacity,
                            stroke, view, (<>))
import Diagrams.TwoD.Text  (FontSlant (..), FontWeight (..),
                            TextAlignment (..))
import Diagrams.Core.Transform (matrixHomRep)

import Diagrams.Backend.PGF.Surface


-- * Types, lenses & runners

-- | Render state, mainly to be used for convienience when build, this module
--   only uses the indent properiy.
data RenderState = RenderState
  { _pos        :: P2   -- ^ Current position
  , _indent     :: Int  -- ^ Current identation
  , _ignoreFill :: Bool
  , _style      :: Style R2
  }

makeLenses ''RenderState

data RenderInfo = RenderInfo
  { _format :: TeXFormat
  , _pprint :: Bool
  }

makeLenses ''RenderInfo

-- | Type wrapper for render monad.
type RenderM m = RWS RenderInfo Blaze.Builder RenderState m

-- | Convienient type for building.
type Render = RenderM ()

-- | Starting state for running the bulider.
initialState :: RenderState
initialState = RenderState
  { _pos        = origin
  , _indent     = 0
  , _ignoreFill = False
  , _style      = lc black mempty -- Until I think of something better:
                                  -- (square 1 # opacity 0.5) doesn't work otherwise
  }

-- | Resets the parts of the state responsible for the drawing stuff
-- eg identation and position is not reset
resetState :: Render
resetState = do
  ignoreFill .= False
  style      .= mempty # lc black
                       # fontSize (Output 1)

renderWith :: Surface -> Bool -> Bool -> (Double,Double) -> Render -> Builder
renderWith s readable standalone bounds r = builder
  where
    (_,builder) = evalRWS r'
                          (RenderInfo (s^.texFormat) readable)
                          initialState
    r' = do
      when standalone $ do
        ln . rawString $ s^.preamble
        maybe (return ())
              (ln . rawString . ($ over both ceiling bounds))
              (s^.pageSize)
        ln . rawString $ s^.beginDoc
      picture $ rectangleBoundingBox bounds >> r
      when standalone $ rawString $ s^.endDoc

----------------------------------------------------------------------

-- builder functions
raw :: ByteString -> Render
raw = tell . Blaze.fromByteString

rawString :: String -> Render
rawString = tell . Blaze.fromString

pgf :: ByteString -> Render
pgf c = raw "\\pgf" >> raw c

rawChar :: Char -> Render
rawChar = tell . Blaze.fromChar

emit :: Render
emit = do
  tab <- use indent
  raw $ B.replicate tab ' '

ln :: Render -> Render
ln r = do
  pp <- view pprint
  if pp
    then emit >> r >> rawChar '\n'
    else r >> rawChar '\n'

-- | Wrap a `Render` in { .. }.
bracers :: Render -> Render
bracers r = do
  rawChar '{'
  r
  rawChar '}'

bracersBlock :: Render -> Render
bracersBlock rs = do
  raw "{\n"
  inBlock rs
  pp <- view pprint
  if pp
    then rawChar '}'
    else emit >> rawChar '}'

-- | Wrap a `Render` in [ .. ].
brackets :: Render -> Render
brackets r = do
  rawChar '['
  r
  rawChar ']'

parens :: Render -> Render
parens r = do
  rawChar '('
  r
  rawChar ')'

-- | Intersperce list of renders with commas.
commaIntersperce :: [Render] -> Render
commaIntersperce = sequence_ . intersperse (rawChar ',')

-- state stuff

-- | Place a render in an indented block
inBlock :: Render -> Render
inBlock r = do
  indent += 2
  r
  indent -= 2

-- * number and points

-- | Render a point.
point :: P2 -> Render
point = tuplePoint . unp2

-- | Render a tuple as a point.
tuplePoint :: (Double,Double) -> Render
tuplePoint (x,y) = do
  pgf "qpoint"
  bracers (bp x)
  bracers (bp y)

-- | Render a double to four decimal places.
n :: Double -> Render
n = raw . toFixed 4

-- | Render length with bp (big point = 1 px at 72 dpi) units.
bp :: Double -> Render
bp = (>> raw "bp") . n

-- | Render length with px units.
px :: Double -> Render
px = (>> raw "px") . n

-- | Render length with mm units.
mm :: Double -> Render
mm = (>> raw "mm") . n -- . (*0.35278)

-- | Render length with pt units.
pt :: Double -> Render
pt = (>> raw "pt") . n -- . (*1.00375)


-- | ε = 0.0001 is the limit at which lines are no longer stroked.
epsilon :: Double
epsilon = 0.0001


-- * PGF environments

picture :: Render -> Render
picture r = do
  beginPicture
  inBlock r
  endPicture

beginPicture :: Render
beginPicture = do
  f <- view format
  ln . raw $ case f of
    LaTeX    -> "\\begin{pgfpicture}"
    ConTeXt  -> "\\startpgfpicture"
    PlainTeX -> "\\pgfpicture"

endPicture :: Render
endPicture = do
  f <- view format
  ln . raw $ case f of
    LaTeX    -> "\\end{pgfpicture}"
    ConTeXt  -> "\\stoppgfpicture"
    PlainTeX -> "\\endpgfpicture"

rectangleBoundingBox :: (Double,Double) -> Render
rectangleBoundingBox bounds = do
  ln $ do
    pgf "pathrectangle"
    bracers $ pgf "pointorigin"
    bracers $ tuplePoint bounds
  ln $ do
    pgf "usepath"
    bracers $ raw "use as bounding box"

-- | Wrap the rendering in a scope.
scope :: Render -> Render
scope r = do
  scopeHeader
  resetState
  inBlock r
  scopeFooter

-- | Header for starting a scope.
scopeHeader :: Render
scopeHeader = do
  f <- view format
  ln . raw $ case f of
    LaTeX    -> "\\begin{pgfscope}"
    ConTeXt  -> "\\startpgfscope"
    PlainTeX -> "\\pgfscope"

-- | Footer for ending a scope.
scopeFooter :: Render
scopeFooter = do
  f <- view format
  ln . raw $ case f of
    LaTeX    -> "\\end{pgfscope}"
    ConTeXt  -> "\\stoppgfscope"
    PlainTeX -> "\\endpgfscope"

-- * Colours

texColor :: Double -> Double -> Double -> Render
texColor r g b = do
  n r
  rawChar ','
  n g
  rawChar ','
  n b

contextColor :: Double -> Double -> Double -> Render
contextColor r g b = do
  raw "r=" >> n r
  rawChar ','
  raw "g=" >> n g
  rawChar ','
  raw "b=" >> n b

-- | Defines an RGB colour with the given name, using the TeX format.
defineColour :: ByteString -> Double -> Double -> Double -> Render
defineColour name r g b = do
  f <- view format
  ln $ case f of
    ConTeXt  -> do
      raw "\\definecolor"
      brackets $ raw name
      brackets $ contextColor r g b
    _        -> do
      raw "\\definecolor"
      bracers $ raw name
      bracers $ raw "rgb"
      bracers $ texColor r g b

parensColor :: Color c => c -> Render
parensColor c = parens $ texColor r g b
  where (r,g,b,_) = colorToSRGBA c

-- | Apply the opacity from a style to a given color.
applyOpacity :: Color c => c -> Style v -> AlphaColour Double
applyOpacity c s = dissolve (maybe 1 getOpacity (getAttr s)) (toAlphaColour c)


-- Path commands

-- | Close the current path.
closePath :: Render
closePath = ln $ pgf "pathclose"

-- | Move path to point.
moveTo :: P2 -> Render
moveTo v = ln $ do
  pos .= v
  pgf "pathmoveto"
  bracers $ point v

-- | Move path by vector.
lineTo :: R2 -> Render
lineTo v = ln $ do
  p <- use pos
  let v' = p .+^ v
  pos .= v'
  pgf "pathlineto"
  bracers $ point v'

-- | Make curved path from vectors.
curveTo :: R2 -> R2 -> R2 -> Render
curveTo v2 v3 v4 = ln $ do
  p <- use pos
  let [v2',v3',v4'] = map (p .+^) [v2,v3,v4]
  pos .= v4'
  pgf "pathcurveto"
  mapM_ (bracers . point) [v2', v3', v4']

-- using paths


-- | Stroke the defined path using parameters from current scope.
stroke :: Render
stroke = ln $ pgf "usepath{stroke}"

-- | Fill the defined path using parameters from current scope.
fill :: Render
fill = ln $ pgf "usepath{fill}"

-- | Use the defined path a clip for everything that follows in the current
--   scope. Stacks.
clip :: Render
clip = ln $ pgf "usepath{clip}"

-- | @usePath fill stroke clip@ combined in one function.
usePath :: Bool -> Bool -> Bool -> Render
usePath False False False      = return ()
usePath doFill doStroke doClip = ln $ do
  pgf "usepath"
  bracers . commaIntersperce . map snd $ filter fst
    [ (doFill,   raw "fill")
    , (doStroke, raw "stroke")
    , (doClip,   raw "clip")
    ]

-- | Uses the current path as the bounding box for whole picture.
asBoundingBox :: Render
asBoundingBox = ln $ do
  pgf "usepath"
  bracers $ raw "use as bounding box"

-- rectangleBoundingBox :: (Double,Double) -> Render
-- rectangleBoundingBox xy = do
--   ln $ do
--     pgf "pathrectangle"
--     bracers $ pgf "pointorigin"
--     bracers $ tuplePoint xy
--   asBoundingBox


-- Line properties

-- | Sets the line width in current scope. Must be done before stroking.
setLineWidth :: Double -> Render
setLineWidth w = ln $ do
  pgf "setlinewidth"
  bracers $ bp w

-- | Sets the line cap in current scope. Must be done before stroking.
setLineCap :: LineCap -> Render
setLineCap cap = ln . pgf $ case cap of
   LineCapButt   -> "setbuttcap"
   LineCapRound  -> "setroundcap"
   LineCapSquare -> "setrectcap"

-- | Sets the line join in current scope. Must be done before stroking.
setLineJoin :: LineJoin -> Render
setLineJoin lJoin = ln . pgf $ case lJoin of
   LineJoinBevel -> "setbeveljoin"
   LineJoinRound -> "setroundjoin"
   LineJoinMiter -> "setmiterjoin"

-- | Sets the miter limit in the current scope. Must be done before stroking.
setMiterLimit :: Double -> Render
setMiterLimit l = do
  pgf "setmiterlimit"
  bracers $ bp l

-- stroke parameters

-- | Sets the dash for the current scope. Must be done before stroking.
setDash :: Dashing -> Render
setDash (Dashing ds offs) = setDash' (map fromOutput ds) (fromOutput offs)


-- \pgfsetdash{{0.5cm}{0.5cm}{0.1cm}{0.2cm}}{0cm}
-- | Takes the dash distances and offset, must be done before stroking.
setDash' :: [Double] -> Double -> Render
setDash' ds off = ln $ do
  pgf "setdash"
  bracers $ mapM_ (bracers . bp) ds
  bracers $ bp off

-- | Sets the stroke colour in current scope. If colour has opacity < 1, the
--   scope opacity is set accordingly. Must be done before stroking.
setLineColor :: (Color c) => c -> Render
setLineColor c = do
  defineColour "sc" r g b
  ln $ pgf "setstrokecolor{sc}"
  --
  when (a /= 1) $ setLineOpacity a
  where
    (r,g,b,a) = colorToSRGBA c

-- | Sets the stroke opacity for the current scope. Should be a value between 0
--   and 1. Must be done  before stroking.
setLineOpacity :: Double -> Render
setLineOpacity a = ln $ do
  pgf "setstrokeopacity"
  bracers $ n a


-- filling

-- | Set the fill rule to winding or even-odd for current scope. Must be done
--   before filling.
setFillRule :: FillRule -> Render
setFillRule rule = ln $ case rule of
  Winding -> pgf "setnonzerorule"
  EvenOdd -> pgf "seteorule"

-- | Sets the fill colour for current scope. If an alpha colour is used, the
--   fill opacity is set accordingly. Must be done before filling.
setFillColor :: (Color c) => c -> Render
setFillColor (colorToSRGBA -> (r,g,b,a)) = do
  defineColour "fc" r g b
  ln $ pgf "setfillcolor{fc}"
  --
  when (a /= 1) $ setFillOpacity a

-- | Sets the stroke opacity for the current scope. Should be a value between 0
--   and 1. Must be done  before stroking.
setFillOpacity :: Double -> Render
setFillOpacity a = ln $ do
  pgf "setfillopacity"
  bracers $ n a


-- transformations

getMatrix :: Transformation R2
          -> (Double, Double, Double, Double, Double, Double)
getMatrix t = (a1,a2,b1,b2,c1,c2)
 where
   [[a1, a2], [b1, b2], [c1, c2]] = matrixHomRep t
  -- (R2 a1 a2) = apply t unitX
  -- (R2 b1 b2) = apply t unitY
  -- (R2 c1 c2) = transl t

-- \pgftransformcm{⟨a⟩}{⟨b⟩}{⟨c⟩}{⟨d⟩}{⟨pointa}

-- | Applies a transformation to the current scope. This transformation only
--   effects coordinates and text, not line withs or dash spacing. (See
--   applyDeepTransform). Must be set before the path is used.
applyTransform :: Transformation R2 -> Render
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
setTransform :: Transformation R2 -> Render
setTransform t = do
  pgf "settransformentries"
  mapM_ (bracers . n) [a, b, c, d] >> mapM_ (bracers . bp) [e, f]
  where
    (a,b,c,d,e,f) = getMatrix t

applyScale :: Double -> Render
applyScale s = ln $ do
  pgf "transformscale"
  bracers $ n s

resetNonTranslations :: Render
resetNonTranslations = ln $ pgf "transformresetnontranslations"

-- | Base transforms are applied by the document reader.
baseTransform :: Transformation R2 -> Render
baseTransform t = do
  pgf "lowlevel"
  bracers $ setTransform t

-- setShadetransform :: Transformation R2 -> Render
-- setShadetransform (dropTransl -> t) = do
--   pgf "setadditionalshadetransform"
--   bracersBlock $ applyTransform t

-- shading

linearGradient :: LGradient -> Render
linearGradient (LGradient stops g0 g1 gt sm) = do
  let d      = transform gt (g0 .-. g1)
      stops' = adjustStops stops sm
  ln $ do
    pgf "declarehorizontalshading"
    bracers $ raw "ft"
    bracers $ raw "100cm" -- must be a better way?
    bracersBlock $ colorSpec (magnitude d) stops'
  shadePath (direction d) $ raw "ft"

radialGradient :: RGradient -> Render
radialGradient (RGradient stops c0 r0 c1 r1 gt sm) = do
  let d = transform gt (c0 .-. c1)
      stops' = adjustStops stops sm
  ln $ do
    pgf "declareradialshading"
    bracers $ raw "ft"
    bracers $ point c0
    bracersBlock $ colorSpec r1 stops'
  shadePath (direction d) $ raw "ft"

-- Dirty adjustments for spread methods (PGF doesn't seem to have them).
adjustStops :: [GradientStop] -> SpreadMethod -> [GradientStop]
adjustStops stops method =
  case method of
    GradPad     -> (stopFraction .~ 0) (head stops)
                 : map (stopFraction +~ 1) stops
                ++ [(stopFraction +~ 2) (last stops)]
    GradReflect -> correct . concat . replicate 10
                 $ [stops, zipWith (\a b -> a & (stopColor .§ b)) stops (reverse stops)]
    GradRepeat  -> correct . replicate 10 $ stops

  where
    correct  = concat . imap (\i -> map (stopFraction +~ (lastStop * fromIntegral i)) )
    lastStop = last stops ^. stopFraction

(.§) :: Lens s t b b -> s -> s -> t
(.§) l a b = b & l #~ (a ^# l)
{-# INLINE (.§) #-}


colorSpec :: Double -> [GradientStop] -> Render
colorSpec d = mapM_ ln
            . combinePairs
            . intersperse (rawChar ';')
            . map mkColor
  where
    mkColor (GradientStop sc sf) = do
      raw "rgb"
      parens $ bp (d*sf)
      raw "="
      parensColor sc

combinePairs :: Monad m => [m a] -> [m a]
combinePairs []  = []
combinePairs [x] = [x]
combinePairs (x1:x2:xs) = (x1 >> x2) : combinePairs xs

shadePath :: Angle -> Render -> Render
shadePath (view deg -> θ) name = ln $ do
  pgf "shadepath"
  bracers name
  bracers $ n θ


-- images

-- \pgfimage[⟨options ⟩]{⟨filename ⟩}

-- | Images are wraped in a \pgftext.
image :: DImage External -> Render
image (DImage (ImageRef path) w h t2) = do
  applyTransform t2
  ln $ do
    pgf "text"
    bracers $ do
      pgf "image"
      brackets $ do
        raw "width=" >> bp (fromIntegral w)
        rawChar ','
        raw "height=" >> bp (fromIntegral h)
      bracers $ rawString path


-- text

renderText :: [Render] -> Render -> Render
renderText ops txt = ln $ do
  pgf "text"
  brackets . commaIntersperce $ ops
  bracers txt

-- | Returns a list of values to be put in square brackets like
--   @\pgftext[left,top]{txt}@.
setTextAlign :: TextAlignment -> [Render]
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

setTextRotation :: Angle -> [Render]
setTextRotation a = case a^.deg of
  0 -> []
  θ -> [raw "rotate=" >> n θ]

-- | Set the font weight by rendering @\bf @. Nothing is done for normal
--   weight.
setFontWeight :: FontWeight -> Render
setFontWeight FontWeightNormal = return ()
setFontWeight FontWeightBold   = raw "\\bf "

-- | Set the font slant by rendering @\bf @. Nothing is done for normal weight.
setFontSlant :: FontSlant -> Render
setFontSlant FontSlantNormal  = return ()
setFontSlant FontSlantItalic  = raw "\\it "
setFontSlant FontSlantOblique = raw "\\sl "

