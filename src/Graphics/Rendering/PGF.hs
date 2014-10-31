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
  -- * Environments
  , scope
  -- , scopeHeader
  -- , resetState
  -- , scopeFooter
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
  , rawString
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
  , opacityGroup
  -- * images
  , image
  -- * Text
  , renderText
  , setTextAlign
  , setTextRotation
  , setFontWeight
  , setFontSlant
  ) where

import           Control.Lens                 (Lens, ifoldMap, makeLenses, use, view,
                                               ( #~ ), (+=), (+~), (-=), (.=), (^#), (^.))
import           Control.Monad.RWS
import           Data.ByteString.Builder
import           Data.ByteString.Char8        (ByteString)
import qualified Data.ByteString.Char8        as B (replicate)
import           Data.List                    (intersperse)
import           Data.Maybe                   (catMaybes)
import           Data.Typeable
import           Numeric

import           Diagrams.Core.Transform      (matrixHomRep)
import           Diagrams.Prelude             hiding (Render, image, moveTo, opacity, stroke, view,
                                               (<>), opacityGroup)
import           Diagrams.TwoD.Text           (FontSlant (..), FontWeight (..), TextAlignment (..))

import           Diagrams.Backend.PGF.Surface


-- * Types, lenses & runners

-- | Render state, mainly to be used for convenience when build, this module
--   only uses the indent properly.
data RenderState n = RenderState
  { _pos        :: P2 n -- ^ Current position
  , _indent     :: Int  -- ^ Current indentation
  , _ignoreFill :: Bool
  , _style      :: Style V2 n
  }

makeLenses ''RenderState

data RenderInfo = RenderInfo
  { _format :: TeXFormat
  , _pprint :: Bool
  }

makeLenses ''RenderInfo

-- | Type for render monad.
type RenderM n m = RWS RenderInfo Builder (RenderState n) m

-- | Convenient type for building.
type Render n = RenderM n ()

-- | Starting state for running the builder.
initialState :: (Typeable n, Floating n) => RenderState n
initialState = RenderState
  { _pos        = origin
  , _indent     = 0
  , _ignoreFill = False
  , _style      = lc black mempty -- Until I think of something better:
                                  -- (square 1 # opacity 0.5) doesn't work otherwise
  }

-- | Resets the parts of the state responsible for the drawing stuff
-- eg indentation and position is not reset
-- resetState :: Render
-- resetState = - do
--   ignoreFill .= False
--   style      .= mempty # lc black
--                        # fontSize (Output 1)

renderWith :: (RealFloat n, Typeable n)
  => Surface -> Bool -> Bool -> V2 n -> Render n -> Builder
renderWith s readable standalone bounds r = builder
  where
    (_,builder) = evalRWS r'
                          (RenderInfo (s^.texFormat) readable)
                          initialState
    r' = do
      when standalone $ do
        ln . rawString $ s^.preamble
        maybe (return ())
              (ln . rawString . ($ fmap ceiling bounds))
              (s^.pageSize)
        ln . rawString $ s^.beginDoc
      picture $ rectangleBoundingBox bounds >> r
      when standalone $ rawString $ s^.endDoc

----------------------------------------------------------------------

-- builder functions
raw :: Builder -> Render n
raw = tell
{-# INLINE raw #-}

rawByteString :: ByteString -> Render n
rawByteString = tell . byteString
{-# INLINE rawByteString #-}

rawString :: String -> Render n
rawString = tell . string8
{-# INLINE rawString #-}

pgf :: Builder -> Render n
pgf c = raw "\\pgf" >> raw c
{-# INLINE pgf #-}

rawChar :: Char -> Render n
rawChar = tell . char8
{-# INLINE rawChar #-}

emit :: Render n
emit = do
  tab <- use indent
  rawByteString $ B.replicate tab ' '
{-# INLINE emit #-}

ln :: Render n -> Render n
ln r = do
  pp <- view pprint
  if pp
    then emit >> r >> rawChar '\n'
    else r >> rawChar '\n'
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
  pp <- view pprint
  if pp
    then rawChar '}'
    else emit >> rawChar '}'

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

-- state stuff

-- | Place a Render n in an indented block
inBlock :: Render n -> Render n
inBlock r = do
  indent += 2
  r
  indent -= 2

-- * number and points

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
n x = rawString $ showFFloat (Just 4) x ""

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

fromReal :: (Real a, Fractional n) => a -> n
fromReal = fromRational . toRational


-- | ε = 0.0001 is the limit at which lines are no longer stroked.
epsilon :: Fractional n => n
epsilon = 0.0001


-- * PGF environments

picture :: Render n -> Render n
picture r = do
  beginPicture
  inBlock r
  endPicture

beginPicture :: Render n
beginPicture = do
  f <- view format
  ln . raw $ case f of
    LaTeX    -> "\\begin{pgfpicture}"
    ConTeXt  -> "\\startpgfpicture"
    PlainTeX -> "\\pgfpicture"

endPicture :: Render n
endPicture = do
  f <- view format
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
  scopeHeader
  -- resetState
  inBlock r
  scopeFooter

-- | Header for starting a scope.
scopeHeader :: Render n
scopeHeader = do
  f <- view format
  ln . raw $ case f of
    LaTeX    -> "\\begin{pgfscope}"
    ConTeXt  -> "\\startpgfscope"
    PlainTeX -> "\\pgfscope"

-- | Footer for ending a scope.
scopeFooter :: Render n
scopeFooter = do
  f <- view format
  ln . raw $ case f of
    LaTeX    -> "\\end{pgfscope}"
    ConTeXt  -> "\\stoppgfscope"
    PlainTeX -> "\\endpgfscope"

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
opacityGroup x r = do
  setFillOpacity x
  transparencyGroup r


-- * Colours

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

-- | Defines an RGB colour with the given name, using the TeX format.
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

-- | Apply the opacity from a style to a given color.
applyOpacity :: Color c => c -> Style V2 n -> AlphaColour Double
applyOpacity c s = dissolve (maybe 1 getOpacity (getAttr s)) (toAlphaColour c)


-- Path commands

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

-- using paths

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

-- | @usePath fill stroke clip@ combined in one function.
usePath :: Bool -> Bool -> Render n
usePath False False     = return ()
usePath doFill doStroke = ln $ do
  pgf "usepathq"
  mapM_ snd $ filter fst
    [ (doFill,   raw "fill")
    , (doStroke, raw "stroke")
    ]

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


-- Line properties

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

-- stroke parameters

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
  when (a /= 1) $ setLineOpacity (fromReal a)
  where
    (r,g,b,a) = colorToSRGBA c

-- | Sets the stroke opacity for the current scope. Should be a value between 0
--   and 1. Must be done  before stroking.
setLineOpacity :: RealFloat n => n -> Render n
setLineOpacity a = ln $ do
  pgf "setstrokeopacity"
  bracers $ n a


-- filling

-- | Set the fill rule to winding or even-odd for current scope. Must be done
--   before filling.
setFillRule :: FillRule -> Render n
setFillRule rule = ln $ case rule of
  Winding -> pgf "setnonzerorule"
  EvenOdd -> pgf "seteorule"

-- | Sets the fill colour for current scope. If an alpha colour is used, the
--   fill opacity is set accordingly. Must be done before filling.
setFillColor :: (RealFloat n, Color c) => c -> Render n
setFillColor (colorToSRGBA -> (r,g,b,a)) = do
  defineColour "fc" r g b
  ln $ pgf "setfillcolor{fc}"
  --
  when (a /= 1) $ setFillOpacity (fromReal a :: Double)

-- | Sets the stroke opacity for the current scope. Should be a value between 0
--   and 1. Must be done  before stroking.
setFillOpacity :: RealFloat a => a -> Render n
setFillOpacity a = ln $ do
  pgf "setfillopacity"
  bracers $ n a


-- transformations

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
baseTransform t = do
  pgf "lowlevel"
  bracers $ setTransform t

-- setShadetransform :: Transformation V2 -> Render n
-- setShadetransform (dropTransl -> t) = do
--   pgf "setadditionalshadetransform"
--   bracersBlock $ applyTransform t

-- shading

linearGradient :: RealFloat n => LGradient n -> Render n
linearGradient (LGradient stops g0 g1 gt sm) = do
  let d      = transform gt (g0 .-. g1)
      stops' = adjustStops stops sm
  ln $ do
    pgf "declarehorizontalshading"
    bracers $ raw "ft"
    bracers $ raw "100cm" -- must be a better way?
    bracersBlock $ colorSpec (norm d) stops'
  shadePath (direction d ^. _theta) $ raw "ft"

radialGradient :: RealFloat n => RGradient n -> Render n
radialGradient (RGradient stops c0 _r0 c1 r1 gt sm) = do
  let d = transform gt (c0 .-. c1)
      stops' = adjustStops stops sm
  ln $ do
    pgf "declareradialshading"
    bracers $ raw "ft"
    bracers $ point c0
    bracersBlock $ colorSpec r1 stops'
  shadePath (direction d ^. _theta) $ raw "ft"

-- Dirty adjustments for spread methods (PGF doesn't seem to have them).
adjustStops :: RealFloat n => [GradientStop n] -> SpreadMethod -> [GradientStop n]
adjustStops stops method =
  case method of
    GradPad     -> (stopFraction .~ 0) (head stops)
                 : map (stopFraction +~ 1) stops
                ++ [(stopFraction +~ 2) (last stops)]
    GradReflect -> correct . concat . replicate 10
                 $ [stops, zipWith (\a b -> a & (stopColor .§ b)) stops (reverse stops)]
    GradRepeat  -> correct . replicate 10 $ stops

  where
    correct  = ifoldMap (\i -> map (stopFraction +~ (lastStop * fromIntegral i)) )
    lastStop = last stops ^. stopFraction

(.§) :: Lens s t b b -> s -> s -> t
(.§) l a b = b & l #~ (a ^# l)
{-# INLINE (.§) #-}


colorSpec :: RealFloat n => n -> [GradientStop n] -> Render n
colorSpec d = mapM_ ln
            . combinePairs
            . intersperse (rawChar ';')
            . map mkColor
  where
    mkColor (GradientStop _sc sf) = do
      raw "rgb"
      parens $ bp (d*sf)
      raw "="
      parensColor _sc

combinePairs :: Monad m => [m a] -> [m a]
combinePairs []  = []
combinePairs [x] = [x]
combinePairs (x1:x2:xs) = (x1 >> x2) : combinePairs xs

shadePath :: RealFloat n => Angle n -> Render n -> Render n
shadePath (view deg -> θ) name = ln $ do
  pgf "shadepath"
  bracers name
  bracers $ n θ


-- images

-- \pgfimage[⟨options ⟩]{⟨filename ⟩}

-- | Images are wraped in a \pgftext.
image :: RealFloat n => DImage n External -> Render n
image (DImage (ImageRef path) w h t2) = do
  applyTransform t2
  ln $ do
    pgf "text"
    bracers $ do
      pgf "image"
      brackets $ do
        raw "width=" >> bp (fromIntegral w :: Double)
        rawChar ','
        raw "height=" >> bp (fromIntegral h :: Double)
      bracers $ rawString path


-- text

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
setFontWeight FontWeightNormal = return ()
setFontWeight FontWeightBold   = raw "\\bf "

-- | Set the font slant by rendering @\bf @. Nothing is done for normal weight.
setFontSlant :: FontSlant -> Render n
setFontSlant FontSlantNormal  = return ()
setFontSlant FontSlantItalic  = raw "\\it "
setFontSlant FontSlantOblique = raw "\\sl "
