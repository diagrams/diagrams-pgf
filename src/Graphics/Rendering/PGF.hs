{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE Rank2Types                 #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.PGF
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
  , initialReader
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
  -- * RenderM commands
  , block
  , emitLn
  , emit
  , raw
  , pgf
  , pgfP
  , bracers
  , bracersL
  , brackets
  , bracketsL
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

import Blaze.ByteString.Builder           as Blaze (Builder,
                                                    fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 as Blaze (fromChar,
                                                    fromString)
import Control.Lens                       (Lens', makeLenses, use, view,
                                           (+=), (-=), (.=), (^.), (+~),
                                           imap, (#~), (^#), Lens)
import Control.Monad.RWS
import Data.ByteString.Char8              (ByteString)
import qualified Data.ByteString.Char8              as B (replicate)
import Data.Double.Conversion.ByteString  (toFixed)
import Data.List                          (intersperse)
import Data.Maybe                         (catMaybes)

import Diagrams.Prelude      hiding (Render, image, moveTo, opacity,
                              stroke, view, (<>))
import Diagrams.TwoD.Text    (FontSlant (..), FontWeight (..),
                              TextAlignment (..))
import Diagrams.TwoD.Types   (R2 (..))

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
    { _surface :: Surface
    , _pprint  :: Bool
    }

makeLenses ''RenderInfo

-- | Type wrapper for render monad.
newtype RenderM m = RenderM { runRender :: RWS RenderInfo Blaze.Builder RenderState m }
-- newtype RenderM m = RenderM (RWS RenderInfo Blaze.Builder RenderState m)
  deriving ( Functor, Monad, Applicative
           , MonadWriter Blaze.Builder
           , MonadState RenderState
           , MonadReader RenderInfo
           )

-- | Convienient type for building.
type Render = RenderM ()

-- | Starting state for running the bulider.
initialState :: RenderState
initialState = RenderState
  { _pos     = origin
  , _indent  = 0
  , _ignoreFill = False
  , _style = lc black mempty -- Until I think of something better:
                             -- (square 1 # opacity 0.5) doesn't work otherwise
  }

-- | Resets the parts of the state responsible for the drawing stuff
-- eg identation and position is not reset
resetState :: Render
resetState = do
    ignoreFill .= False
    style      .= mempty # lc black
                         # fontSize (Output 1)

-- | Starting RenderInfo.
initialReader :: Surface -> RenderInfo
initialReader s = RenderInfo s False

-- | Lens for accessing the TeX format of a surface
format :: Lens' RenderInfo TeXFormat
format = surface . texFormat

-- txtTrans :: Lens' RenderInfo Bool
-- txtTrans = surface . textTransforms

pdfPageBounds :: (Double,Double) -> (Double, Double) -> Render
pdfPageBounds (x,y) (x0,y0) = do
  emitLn $ raw "\\pdfpagewidth="  >> bp x
  emitLn $ raw "\\pdfpageheight=" >> bp y
  emitLn $ raw "\\pdfhorigin="    >> bp x0
  emitLn $ raw "\\pdfvorigin="    >> bp y0

renderWith :: Surface -> Bool -> Bool -> (Double,Double) -> Render -> Builder
renderWith s readable standalone bounds r = builder
  where
    (_,builder) = evalRWS r'
                          (RenderInfo s readable)
                          initialState
    r' = runRender $ do
      when standalone $ do
        emitLn . rawString $ s^.preamble
        maybe (return ()) (pdfPageBounds bounds) (s^.pdfOrigin)
        maybe (return ()) (rawString . ($ bounds)) (s^.pageSize)
        emitLn . rawString $ s^.beginDoc
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

emitLn :: Render -> Render
emitLn r = do
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

-- | Wrap each element in { .. } and put them next to each other.
bracersL :: [Render] -> Render
bracersL = mapM_ bracers

-- | Intersperce list with commas and wrap in { .. }. If the list is
--   empty, {} is rendered.
bracersL' :: [Render] -> Render
bracersL' = bracers . commaIntersperce

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

-- | Intersperce list with commas and wrap in [ .. ]. If the list is
--   empty, nothing is rendered.
bracketsL :: [Render] -> Render
bracketsL [] = return ()
bracketsL rs = brackets . commaIntersperce $ rs

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

pgfP :: R2 -> Render
pgfP = pgfP' . unr2

pgfPoint :: P2 -> Render
pgfPoint = pgfP' . unp2

pgfP' :: (Double,Double) -> Render
pgfP' (x,y) = do
  pgf "qpoint"
  bracers (bp x)
  bracers (bp y)

show4 :: Double -> Render
show4 = raw . toFixed 4

bp :: Double -> Render
bp = (>> raw "bp") . show4

-- px :: Double -> Render
-- px = (>> raw "px") . show4

-- mm :: Double -> Render
-- mm = (>> raw "mm") . show4 . (*0.35277)

-- pt :: Double -> Render
-- pt = (>> raw "pt") . show4


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
  emitLn . raw $ case f of
    LaTeX    -> "\\begin{pgfpicture}"
    ConTeXt  -> "\\startpgfpicture"
    PlainTeX -> "\\pgfpicture"

endPicture :: Render
endPicture = do
  f <- view format
  emitLn . raw $ case f of
    LaTeX    -> "\\end{pgfpicture}"
    ConTeXt  -> "\\stoppgfpicture"
    PlainTeX -> "\\endpgfpicture"

rectangleBoundingBox :: (Double,Double) -> Render
rectangleBoundingBox bounds = do
  emitLn $ do
    pgf "pathrectangle"
    bracers $ pgf "pointorigin"
    bracers $ pgfP' bounds
  emitLn $ do
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
  emitLn . raw $ case f of
    LaTeX    -> "\\begin{pgfscope}"
    ConTeXt  -> "\\startpgfscope"
    PlainTeX -> "\\pgfscope"

-- | Footer for ending a scope.
scopeFooter :: Render
scopeFooter = do
  f <- view format
  emitLn . raw $ case f of
    LaTeX    -> "\\end{pgfscope}"
    ConTeXt  -> "\\stoppgfscope"
    PlainTeX -> "\\endpgfscope"

-- * Colours

texColor :: Double -> Double -> Double -> Render
texColor r g b = do
  show4 r
  rawChar ','
  show4 g
  rawChar ','
  show4 b

contextColor :: Double -> Double -> Double -> Render
contextColor r g b = do
  raw "r=" >> show4 r
  rawChar ','
  raw "g=" >> show4 g
  rawChar ','
  raw "b=" >> show4 b

-- | Defines an RGB colour with the given name, using the TeX format.
defineColour :: ByteString -> Double -> Double -> Double -> Render
defineColour name r g b = do
  f <- view format
  emitLn $ case f of
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
parensColor c = parens $ do
  show4 r >> rawChar ','
  show4 g >> rawChar ','
  show4 b
  where (r,g,b,_) = colorToSRGBA c

-- | Apply the opacity from a style to a given color.
applyOpacity :: Color c => c -> Style v -> AlphaColour Double
applyOpacity c s = dissolve (maybe 1 getOpacity (getAttr s)) (toAlphaColour c)


-- Path commands

closePath :: Render
closePath = emitLn $ pgf "pathclose"

moveTo :: P2 -> Render
moveTo v = emitLn $ do
  pos .= v
  pgf "pathmoveto"
  bracers $ pgfPoint v

lineTo :: R2 -> Render
lineTo v = emitLn $ do
  p <- use pos
  let v' = p .+^ v
  pos .= v'
  pgf "pathlineto"
  bracers $ pgfPoint v'

curveTo :: R2 -> R2 -> R2 -> Render
curveTo v2 v3 v4 = emitLn $ do
  p <- use pos
  let [v2',v3',v4'] = map (p .+^) [v2,v3,v4]
  pos .= v4'
  pgf "pathcurveto"
  bracersL $ map pgfPoint [v2', v3', v4']

-- using paths


-- | Stroke the defined path using parameters from current scope.
stroke :: Render
stroke = emitLn $ pgf "usepath{stroke}"

-- | Fill the defined path using parameters from current scope.
fill :: Render
fill = emitLn $ pgf "usepath{fill}"

-- | Use the defined path a clip for everything that follows in the current
--   scope. Stacks.
clip :: Render
clip = emitLn $ pgf "usepath{clip}"

-- | @usePath fill stroke clip@ combined in one function.
usePath :: Bool -> Bool -> Bool -> Render
usePath False False False = return ()
usePath doFill doStroke doClip = emitLn $ do
  pgf "usepath"
  bracersL' . map snd $ filter fst
            [ (doFill,   raw "fill")
            , (doStroke, raw "stroke")
            , (doClip,   raw "clip")
            ]


-- | Uses the current path as the bounding box for whole picture.
asBoundingBox :: Render
asBoundingBox = emitLn $ do
  pgf "usepath"
  bracers $ raw "use as bounding box"

-- rectangleBoundingBox :: (Double,Double) -> Render
-- rectangleBoundingBox xy = do
--   emitLn $ do
--     pgf "pathrectangle"
--     bracers $ pgf "pointorigin"
--     bracers $ pgfP' xy
--   asBoundingBox


-- Line properties

-- | Sets the line width in current scope. Must be done before stroking.
setLineWidth :: Double -> Render
setLineWidth w = emitLn $ do
  pgf "setlinewidth"
  bracers $ bp w

-- | Sets the line cap in current scope. Must be done before stroking.
setLineCap :: LineCap -> Render
setLineCap cap = emitLn . pgf $ case cap of
   LineCapButt   -> "setbuttcap"
   LineCapRound  -> "setroundcap"
   LineCapSquare -> "setrectcap"

-- | Sets the line join in current scope. Must be done before stroking.
setLineJoin :: LineJoin -> Render
setLineJoin lJoin = emitLn . pgf $ case lJoin of
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
setDash' ds off = emitLn $ do
  pgf "setdash"
  bracers . bracersL $ map bp ds
  bracers $ bp off

-- | Sets the stroke colour in current scope. If colour has opacity < 1, the
--   scope opacity is set accordingly. Must be done before stroking.
setLineColor :: (Color c) => c -> Render
setLineColor c = do
  defineColour "sc" r g b
  emitLn $ pgf "setstrokecolor{sc}"
  --
  when (a /= 1) $ setLineOpacity a
  where
    (r,g,b,a) = colorToSRGBA c

-- | Sets the stroke opacity for the current scope. Should be a value between 0
--   and 1. Must be done  before stroking.
setLineOpacity :: Double -> Render
setLineOpacity a = emitLn $ do
  pgf "setstrokeopacity"
  bracers $ show4 a


-- filling

-- | Set the fill rule to winding or even-odd for current scope. Must be done
--   before filling.
setFillRule :: FillRule -> Render
setFillRule rule = emitLn $ case rule of
  Winding -> pgf "setnonzerorule"
  EvenOdd -> pgf "seteorule"

-- | Sets the fill colour for current scope. If an alpha colour is used, the
--   fill opacity is set accordingly. Must be done before filling.
setFillColor :: (Color c) => c -> Render
setFillColor (colorToSRGBA -> (r,g,b,a)) = do
  defineColour "fc" r g b
  emitLn $ pgf "setfillcolor{fc}"
  --
  when (a /= 1) $ setFillOpacity a

-- | Sets the stroke opacity for the current scope. Should be a value between 0
--   and 1. Must be done  before stroking.
setFillOpacity :: Double -> Render
setFillOpacity a = emitLn $ do
  pgf "setfillopacity"
  bracers $ show4 a


-- transformations

getMatrix :: Transformation R2
          -> (Double, Double, Double, Double, Double, Double)
getMatrix t = (a1,a2,b1,b2,c1,c2)
 where
  (R2 a1 a2) = apply t unitX
  (R2 b1 b2) = apply t unitY
  (R2 c1 c2) = transl t

-- \pgftransformcm{⟨a⟩}{⟨b⟩}{⟨c⟩}{⟨d⟩}{⟨pointa}

-- | Applies a transformation to the current scope. This transformation only
--   effects coordinates and text, not line withs or dash spacing. (See
--   applyDeepTransform). Must be set before the path is used.
applyTransform :: Transformation R2 -> Render
applyTransform t
  | isID      = return ()
  | shiftOnly = emitLn $ do
  -- | otherwise = emitLn $ do
      pgf "transformshift"
      bracers p
  | otherwise = emitLn $ do
    pgf "transformcm"
    bracersL $ map show4 [a, b, c, d] ++ [p]
  where
    (a,b,c,d,e,f) = getMatrix t
    p = pgfP' (e,f)
    --
    shiftOnly = (a,b,c,d) == (1,0,0,1)
    isID      = shiftOnly && (e,f) == (0,0)

-- | Resets the transform and sets it. Must be set before the path is used.
setTransform :: Transformation R2 -> Render
setTransform t = do
  pgf "settransformentries"
  bracersL $ map show4 [a, b, c, d] ++ map bp [e, f]
  where
    (a,b,c,d,e,f) = getMatrix t

applyScale :: Double -> Render
applyScale s = emitLn $ do
  pgf "transformscale"
  bracers $ show4 s

resetNonTranslations :: Render
resetNonTranslations = emitLn $ pgf "transformresetnontranslations"

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
  emitLn $ do
    pgf "declarehorizontalshading"
    bracers $ raw "ft"
    bracers $ raw "100cm" -- must be a better way?
    bracersBlock $ colorSpec (magnitude d) stops'
  shadePath (direction d) $ raw "ft"

radialGradient :: RGradient -> Render
radialGradient (RGradient stops c0 r0 c1 r1 gt sm) = do
  let d = transform gt (c0 .-. c1)
      stops' = adjustStops stops sm
  emitLn $ do
    pgf "declareradialshading"
    bracers $ raw "ft"
    bracers $ pgfPoint c0
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
colorSpec d = mapM_ emitLn
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
shadePath (view deg -> θ) name = emitLn $ do
  pgf "shadepath"
  bracers name
  bracers $ show4 θ


-- images

-- \pgfimage[⟨options ⟩]{⟨filename ⟩}

-- | Images are wraped in a \pgftext.
image :: DImage External -> Render
image (DImage (ImageRef path) w h t2) = do
  applyTransform t2
  emitLn $ do
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
renderText ops txt = emitLn $ do
  pgf "text"
  bracketsL ops
  bracers txt

-- | Returns a list of values to be put in square brackets like
--   @\pgftext[left,top]{txt}@. Usually to be used with @bracketsL@.
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
  θ -> [raw "rotate=" >> show4 θ]

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

