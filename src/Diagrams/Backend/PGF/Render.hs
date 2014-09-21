{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Diagrams.Backend.PGF.Render
  ( PGF (..)
  , Options (..)

  -- * Lenses
  , surface
  , sizeSpec
  , readable
  , standalone

  -- * Utilities
  , escapeString
  ) where

import           Control.Lens                 (Lens', lens, op, use, uses, (.=), (<<<>=), (^.))
import           Control.Lens.Extras          (is)
import           Control.Monad                (when)
import           Data.ByteString.Builder

import           Data.Default
import           Data.Foldable                (foldMap)
import           Data.Functor
import           Data.Hashable                (Hashable (..))
import           Data.Maybe                   (fromMaybe)
import           Data.Tree                    (Tree (Node))

import           Diagrams.Core.Types
import           Diagrams.Prelude

import           Diagrams.Backend.PGF.Hbox    (Hbox (..))
import           Diagrams.Backend.PGF.Surface (Surface)
import           Diagrams.TwoD.Adjust         (adjustDia2D)
import           Diagrams.TwoD.Path
import           Diagrams.TwoD.Size           (sizePair)
import           Diagrams.TwoD.Text           (Text (..), TextAlignment (..), getFontSize,
                                               getFontSizeIsLocal, getFontSlant, getFontWeight)

import           Data.Data
import qualified Graphics.Rendering.PGF       as P

-- | This data declaration is simply used as a token to distinguish
--   this rendering engine.
data PGF = PGF
  deriving (Show, Typeable)

instance DataFloat n => Backend PGF V2 n where
  data Render  PGF V2 n = R (P.Render n)
  type Result  PGF V2 n = Builder
  data Options PGF V2 n = PGFOptions
    { _surface    :: Surface      -- ^ Surface you want to use.
    , _sizeSpec   :: SizeSpec2D n -- ^ The requested size.
    , _readable   :: Bool         -- ^ Indented lines for @.tex@ output.
    , _standalone :: Bool         -- ^ Should @.tex@ output be standalone.
    }

  renderRTree _ ops rt =
    P.renderWith (ops^.surface) (ops^.readable) (ops^.standalone) bounds r
      where
        (R r)  = toRender rt
        bounds = sizePair (ops^.sizeSpec)

  adjustDia = adjustDia2D sizeSpec

toRender :: (OrderedField n, RealFloat n, Typeable n) => RTree PGF V2 n a -> Render PGF V2 n
toRender (Node (RPrim p) _)     = render PGF p
toRender (Node (RStyle sty) rs) = R . P.scope $ do
  oldSty <- P.style <<<>= sty
  P.ignoreFill .= False

  setClipPaths <~ op Clip
  let R r = foldMap toRender rs
  pgf <- r

  P.style .= oldSty

  return pgf
-- toRender (Node (RAnnot (Href uri)) rs)
--   = R $ do
--       let R r = foldMap fromRTree rs
--       pgf <- r
--       return $ (S.a ! xlinkHref (S.toValue uri)) svg
toRender (Node _ rs)            = foldMap toRender rs


renderP :: (Renderable a PGF, Vn a ~ V2 n) => a -> P.Render n
renderP (render PGF -> R r) = r

instance Fractional n => Default (Options PGF V2 n) where
  def = PGFOptions
          { _surface    = def
          , _sizeSpec   = Absolute
          , _readable   = True
          , _standalone = False
          }

instance Monoid (Render PGF V2 n) where
  mempty                  = R $ return ()
  (R ra) `mappend` (R rb) = R (ra >> rb)

-- | Lens onto the surface used to render.
surface :: Lens' (Options PGF V2 n) Surface
surface = lens getter setter
  where getter (PGFOptions { _surface = s }) = s
        setter o s = o { _surface = s }

-- | Lens onto whether a standalone TeX document should be produced.
standalone :: Lens' (Options PGF V2 n) Bool
standalone = lens getter setter
  where getter (PGFOptions { _standalone = s }) = s
        setter o s = o { _standalone = s }

-- | Lens onto the 'SizeSpec2D'.
sizeSpec :: Lens' (Options PGF V2 n) (SizeSpec2D n)
sizeSpec = lens getSize setSize
  where getSize (PGFOptions { _sizeSpec = s }) = s
        setSize o s = o { _sizeSpec = s }

-- | Lens onto whether the lines of the TeX output are indented.
readable :: Lens' (Options PGF V2 n) Bool
readable = lens getR setR
  where getR (PGFOptions { _readable = r }) = r
        setR o r = o { _readable = r }

-- | Use the path that has already been drawn in scope. The path is stroked if
--   linewidth > 0.0001 and if filled if a colour is defined.
--
--   All stroke and fill properties from the current @style@ are also output here.
draw :: (TypeableFloat n) => P.Render n
draw = do
  mFillTexture <- (getFillTexture <$>) . getAttr <$> use P.style
  canFill      <- uses P.ignoreFill not
  doFill       <- case mFillTexture of
    Nothing -> return False
    Just t  -> setFillTexture t >> return (is _SC t && canFill)
  when doFill $
    P.setFillRule <~ getFillRule
  --
  doStroke <- shouldStroke
  when doStroke $ do
    setLineTexture <~ getLineTexture -- stoke opacity needs to be set
    P.setLineJoin  <~ getLineJoin
    P.setLineWidth <~ fromOutput . getLineWidth
    P.setLineCap   <~ getLineCap
    P.setDash      <~ getDashing
  --
  P.usePath doFill doStroke False

-- helper function to easily get options and set them
(<~) :: (AttributeClass a) => (b -> P.Render n) -> (a -> b) -> P.Render n
renderF <~ getF = do
  s <- use P.style
  let mAttr = (getF <$>) . getAttr $ s
  maybe (return ()) renderF mAttr

infixr 2 <~

setFillTexture :: RealFloat n => Texture n -> P.Render n
setFillTexture t = case t of
  (SC (SomeColor c)) -> setFillColor' c
  (LG g)             -> P.linearGradient g
  (RG g)             -> P.radialGradient g

setFillColor' :: (RealFloat n, Color c) => c -> P.Render n
setFillColor' c = do
  s <- use P.style
  P.setFillColor $ applyOpacity c s

setLineColor' :: (RealFloat n, Color c) => c -> P.Render n
setLineColor' c = do
  s <- use P.style
  P.setLineColor $ applyOpacity c s

setLineTexture :: RealFloat n => Texture n -> P.Render n
setLineTexture (SC (SomeColor c)) = setLineColor' c
setLineTexture _                  = return ()

-- | Apply the opacity from a style to a given color.
applyOpacity :: Color c => c -> Style v n -> AlphaColour Double
applyOpacity c s = dissolve (maybe 1 getOpacity (getAttr s)) (toAlphaColour c)

getNumAttr :: AttributeClass (a n) => (a n -> t) -> Style V2 n -> Maybe t
getNumAttr f = (f <$>) . getAttr

getMeasuredAttr :: (AttributeClass (a n), Num t) => (a n -> Measure t) -> Style V2 n -> Maybe t
getMeasuredAttr f = (fromOutput . f <$>) . getAttr

-- | Queries the current style and decides if the path should be stroked. Paths
--   are stroked when lw > 0.0001
shouldStroke :: (OrderedField n, Typeable n) => P.RenderM n Bool
shouldStroke = do
  -- mLWidth <- (fromOutput . getLineWidth <$>) . getAttr <$> use P.style
  mLWidth <- getMeasuredAttr getLineWidth <$> use P.style
  --
  return $ maybe True (> P.epsilon) mLWidth

setClipPaths :: TypeableFloat n => [Path V2 n] -> P.Render n
setClipPaths = mapM_ setClipPath

setClipPath :: TypeableFloat n => Path V2 n -> P.Render n
setClipPath (Path trs) = do
  mapM_ renderTrail trs
  P.clip
  where
    renderTrail (viewLoc -> (p, tr)) = do
      P.moveTo p
      renderP tr

renderPath :: (OrderedField n, Typeable n, RealFloat n) => Path V2 n -> P.Render n
renderPath (Path trs) = do
  when (any (isLine . unLoc) trs) $ P.ignoreFill .= True
  mapM_ renderTrail trs
  draw
  where
    renderTrail (viewLoc -> (p, tr)) = do
      P.moveTo p
      renderP tr

-- | Escapes some common characters in a string.
escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar ch = case ch of
      '$' -> "\\$"
      '%' -> "\\letterpercent{}"
      '&' -> "\\&"
      '#' -> "\\#"
      '_' -> "\\_"
      '{' -> "$\\{$"
      '}' -> "$\\}$"
      '\\'-> "$\\backslash{}$"
      '~' -> "\\~{}"
      '^' -> "\\^{}"
      '[' -> "{[}"
      ']' -> "{]}"
      x   -> [x]

-- | Renders text. Colour is set by fill colour. Opacity is inheritied from
--   scope fill opacity. Does not support full alignment. Text is not escaped.
renderText :: (RealFloat n, Typeable n) => Text n -> P.Render n
renderText (Text tt tn txtAlign str) = do
  -- isLocal <- (getFontSizeIsLocal <$>) . getAttr <$> use P.style
  isLocal <- getNumAttr getFontSizeIsLocal <$> use P.style
  setFillTexture <~ getFillTexture
  --
  -- doTxtTrans <- view P.txtTrans
  P.applyTransform (if fromMaybe False isLocal then tt else tn)
  -- if doTxtTrans
  (P.applyScale . (/8)) <~ fromOutput . getFontSize
      -- (/8) was obtained from trail and error
    -- else P.resetNonTranslations
  --
  P.renderText (P.setTextAlign txtAlign) $ do
    P.setFontWeight <~ getFontWeight
    P.setFontSlant  <~ getFontSlant
    P.rawString str

renderHbox :: RealFloat n => Hbox n -> P.Render n
renderHbox (Hbox tt str) = do
  -- isLocal <- (getFontSizeIsLocal <$>) . getAttr <$> use P.style
  P.applyTransform tt -- (if fromMaybe False isLocal then tn else tt)
  -- P.applyScale 8
  -- P.resetNonTranslations
  P.renderText (P.setTextAlign BaselineText) (P.rawString str)

------------------------------------------------------------------------
-- Renderable instances

instance TypeableFloat n => Renderable (Segment Closed V2 n) PGF where
  render _ (Linear (OffsetClosed v))       = R $ P.lineTo v
  render _ (Cubic v1 v2 (OffsetClosed v3)) = R $ P.curveTo v1 v2 v3

instance TypeableFloat n => Renderable (Trail V2 n) PGF where
  render _ t = withLine (render' . lineSegments) t
    where
      render' segs = R $ do
        mapM_ renderP segs
        when (isLoop t) P.closePath

instance TypeableFloat n => Renderable (Path V2 n) PGF where
  render _ = R . renderPath

instance TypeableFloat n => Renderable (Text n) PGF where
  render _ = R . renderText

instance TypeableFloat n => Renderable (Hbox n) PGF where
  render _ = R . renderHbox

-- | Supported: @.pdf@, @.jpg@, @.png@.
instance RealFloat n => Renderable (DImage n External) PGF where
  render _  = R . P.image

------------------------------------------------------------------------
-- Hashable instances

instance Hashable n => Hashable (Options PGF V2 n) where
  hashWithSalt s (PGFOptions sf sz rd st)
    = s  `hashWithSalt`
      sf `hashWithSalt`
      sz `hashWithSalt`
      rd `hashWithSalt`
      st

