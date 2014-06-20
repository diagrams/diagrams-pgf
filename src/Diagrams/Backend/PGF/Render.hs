{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
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
  ) where

import Blaze.ByteString.Builder (Builder)
import Control.Lens             (Lens', isn't, lens, op, use, (.=),
                                 (<<%=), (^.))
import Control.Monad            (when)
-- import Control.Monad.StateStack

import Data.Default
import Data.Foldable (foldMap)
import Data.Functor
import Data.Hashable (Hashable (..))
import Data.Maybe    (fromMaybe)
import Data.Tree     (Tree (Node))
import Data.Typeable (Typeable)

import Diagrams.Core.Types
import Diagrams.Prelude

import Diagrams.Backend.PGF.Hbox    (Hbox (..))
import Diagrams.Backend.PGF.Surface (Surface)
import Diagrams.TwoD.Adjust         (adjustDiaSize2D)
import Diagrams.TwoD.Path
import Diagrams.TwoD.Size           (sizePair)
import Diagrams.TwoD.Text           (Text (..), TextAlignment (..),
                                     getFontSize, getFontSizeIsLocal,
                                     getFontSlant, getFontWeight)

import qualified Graphics.Rendering.PGF as P

-- | This data declaration is simply used as a token to distinguish
--   this rendering engine.
data PGF = PGF
  deriving (Show, Typeable)

-- type PGFStack a = StateStackT PGFState P.RenderM a

instance Backend PGF R2 where
  data Render  PGF R2 = P (P.Render)
  type Result  PGF R2 = Builder
  data Options PGF R2 = PGFOptions
    { _surface    :: Surface    -- ^ Surface you want to use.
    , _sizeSpec   :: SizeSpec2D -- ^ The requested size.
    , _readable   :: Bool       -- ^ Pretty print output.
    , _standalone :: Bool       -- ^ Should .tex output be standalone.
    }

  renderRTree _ ops rt =
    P.renderWith (ops^.surface) (ops^.readable) (ops^.standalone) bounds r
      where
        (P r)  = toRender rt
        bounds = sizePair (ops^.sizeSpec)

  adjustDia = adjustDiaSize2D sizeSpec

toRender :: RTree PGF R2 a -> Render PGF R2
toRender (Node (RPrim p) _) = render PGF p
toRender (Node (RStyle sty) rs) = P . P.scope $ do
  oldSty <- P.style <<%= (<> sty)

  setClipPaths <~ op Clip
  let P r = foldMap toRender rs
  pgf <- r

  P.style      .= oldSty
  P.ignoreFill .= False
  return pgf
-- toRender (Node (RAnnot (Href uri)) rs)
--   = R $ do
--       let R r = foldMap fromRTree rs
--       pgf <- r
--       return $ (S.a ! xlinkHref (S.toValue uri)) svg
toRender (Node _ rs) = foldMap toRender rs

-- toRender :: RTree PGF R2 Annotation -> Render PGF R2
-- toRender = fromRTree
--   -- . Node (RStyle (mempty # recommendFillColor (transparent :: AlphaColour Double)))
--   -- . (:[])
--     where
--       fromRTree (Node (RPrim p) _)     = render PGF p
--       fromRTree (Node (RStyle sty) rs) = P . P.scope $ do
--         let P r = foldMap fromRTree rs
--         P.style %= (<> sty)
--         setClipPaths <~ op Clip
--         pgf <- r
--         P.resetState
--         return pgf
--       fromRTree (Node _ rs)            = foldMap fromRTree rs

renderP :: (Renderable a PGF, V a ~ R2) => a -> P.Render
renderP (render PGF -> P r) = r

instance Default (Options PGF R2) where
  def = PGFOptions
          { _surface    = def
          , _sizeSpec   = Absolute
          , _readable   = True
          , _standalone = False
          }

instance Monoid (Render PGF R2) where
  mempty                  = P $ return ()
  (P ra) `mappend` (P rb) = P (ra >> rb)

-- | Lens onto the surface used to render.
surface :: Lens' (Options PGF R2) Surface
surface = lens getter setter
  where getter (PGFOptions { _surface = s }) = s
        setter o s = o { _surface = s }

-- | Lens onto whether a standalone TeX document should be produced.
standalone :: Lens' (Options PGF R2) Bool
standalone = lens getter setter
  where getter (PGFOptions { _standalone = s }) = s
        setter o s = o { _standalone = s }

-- | Lens onto the 'SizeSpec2D'.
sizeSpec :: Lens' (Options PGF R2) SizeSpec2D
sizeSpec = lens getSize setSize
  where getSize (PGFOptions { _sizeSpec = s }) = s
        setSize o s = o { _sizeSpec = s }

-- | Lens onto whether the lines of the TeX output are indented.
readable :: Lens' (Options PGF R2) Bool
readable = lens getR setR
  where getR (PGFOptions { _readable = r }) = r
        setR o r = o { _readable = r }

-- defStyle :: Style R2
-- defStyle = mempty # lineWidthA def # lineColorA def
--                   # lineCap def # lineJoin def
--                   # lineMiterLimitA def

-- set default values outside scope
-- initialStyle :: P.Render
-- initialStyle = do
--   P.setLineWidth <~ getLineWidth
--   P.setLineColor <~ getLineColor
--   P.setLineCap   <~ getLineCap
--   P.setLineJoin  <~ getLineJoin

-- | Use the path that has already been drawn in scope. The path is stroked if
--   linewidth > 0.0001 and if filled if a colour is defined.
--
--   All stroke and fill properties from the current @style@ are also output here.
draw :: P.Render
draw = do
  mFillTexture <- (getFillTexture <$>) . getAttr <$> use P.style
  doFill       <- case mFillTexture of
    Nothing -> return False
    Just t  -> setFillTexture t >> return (not $ isn't _SC t)
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
(<~) :: (AttributeClass a) => (b -> P.Render) -> (a -> b) -> P.RenderM ()
renderF <~ getF = do
  s <- use P.style
  let mAttr = (getF <$>) . getAttr $ s
  maybe (return ()) renderF mAttr

infixr 2 <~

setFillTexture :: Texture -> P.Render
setFillTexture t = case t of
  (SC (SomeColor c)) -> setFillColor' c
  (LG g)             -> P.linearGradient g
  (RG g)             -> P.radialGradient g

setFillColor' :: (Color c) => c -> P.Render
setFillColor' c = do
  s <- use P.style
  P.setFillColor $ applyOpacity c s

setLineColor' :: (Color c) => c -> P.Render
setLineColor' c = do
  s <- use P.style
  P.setLineColor $ applyOpacity c s

setLineTexture :: Texture -> P.Render
setLineTexture (SC (SomeColor c)) = setLineColor' c
setLineTexture _                  = return ()

-- | Apply the opacity from a style to a given color.
applyOpacity :: Color c => c -> Style v -> AlphaColour Double
applyOpacity c s = dissolve (maybe 1 getOpacity (getAttr s)) (toAlphaColour c)

-- | Queries the current style and decides if the path should be stroked. Paths
--   are stroked when lw > 0.0001
shouldStroke :: P.RenderM Bool
shouldStroke = do
  mLWidth <- (fromOutput . getLineWidth <$>) . getAttr <$> use P.style
  --
  return $ maybe True (> P.epsilon) mLWidth

setClipPaths :: [Path R2] -> P.Render
setClipPaths = mapM_ setClipPath

setClipPath :: Path R2 -> P.Render
setClipPath (Path trs) = do
  mapM_ renderTrail trs
  P.clip
  where
    renderTrail (viewLoc -> (p, tr)) = do
      P.moveTo p
      renderP tr

renderPath :: Path R2 -> P.Render
renderPath (Path trs) = do
  when (any (isLine . unLoc) trs) $ P.ignoreFill .= True
  mapM_ renderTrail trs
  draw
  where
    renderTrail (viewLoc -> (p, tr)) = do
      P.moveTo p
      renderP tr

-- | Escapes some common charcters in a string. Lots of things don't work in
--   plain TeX.
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
renderText :: Text -> P.Render
renderText (Text tt tn txtAlign str) = do
  isLocal <- (getFontSizeIsLocal <$>) . getAttr <$> use P.style
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
    P.rawString $ escapeString str

renderHbox :: Hbox -> P.Render
renderHbox (Hbox tt str) = do
  -- isLocal <- (getFontSizeIsLocal <$>) . getAttr <$> use P.style
  P.applyTransform tt -- (if fromMaybe False isLocal then tn else tt)
  -- P.applyScale 8
  -- P.resetNonTranslations
  P.renderText (P.setTextAlign BaselineText) (P.rawString str)

------------------------------------------------------------------------
-- Renderable instances

instance Renderable (Segment Closed R2) PGF where
  render _ (Linear (OffsetClosed v))       = P $ P.lineTo v
  render _ (Cubic v1 v2 (OffsetClosed v3)) = P $ P.curveTo v1 v2 v3

instance Renderable (Trail R2) PGF where
  render _ t = withLine (render' . lineSegments) t
    where
      render' segs = P $ do
        mapM_ renderP segs
        when (isLoop t) P.closePath

instance Renderable (Path R2) PGF where
  render _ = P . renderPath

instance Renderable Text PGF where
  render _ = P . renderText

instance Renderable Hbox PGF where
  render _ = P . renderHbox

-- | Supported: @.pdf@, @.jpg@, @.png@.
instance Renderable (DImage External) PGF where
  render _  = P . P.image

------------------------------------------------------------------------
-- Hashable instances

instance Hashable (Options PGF R2) where
  hashWithSalt s (PGFOptions sf sz rd st)
    = s  `hashWithSalt`
      sf `hashWithSalt`
      sz `hashWithSalt`
      rd `hashWithSalt`
      st

