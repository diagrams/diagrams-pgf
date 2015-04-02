{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.PGF.Render
-- Copyright   :  (c) 2015 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This is an internal module exposeing internals for rendering a
-- diagram. This is for advanced use only. 'Diagrams.Backend.PGF'
-- has enought for general use.
--
module Diagrams.Backend.PGF.Render
  ( PGF (..)
  , Options (..)
  , Render (..)

  -- * Lenses
  , surface
  , sizeSpec
  , readable
  , standalone

  -- * Utilities
  , escapeString
  ) where

import           Control.Monad                (when)
import           Data.ByteString.Builder

import qualified Data.Foldable                as F (foldMap)
import           Data.Functor
import           Data.Hashable                (Hashable (..))
import           Data.Tree                    (Tree (Node))

import           Diagrams.Core.Types
import           Diagrams.Prelude hiding ((<~))

import           Diagrams.Backend.PGF.Hbox    (Hbox (..))
import           Diagrams.Backend.PGF.Surface (Surface)
import           Diagrams.TwoD.Adjust         (adjustDia2D)
import           Diagrams.TwoD.Path
import           Diagrams.TwoD.Text           (Text (..), TextAlignment (..), getFontSize,
                                               getFontSlant, getFontWeight)
import           Data.Typeable
import qualified Graphics.Rendering.PGF       as P

import Prelude

-- | This data declaration is simply used as a token to distinguish
--   this rendering engine.
data PGF = PGF
  deriving (Show, Typeable)

instance TypeableFloat n => Backend PGF V2 n where
  newtype Render  PGF V2 n = R (P.Render n)
  type    Result  PGF V2 n = Builder
  data    Options PGF V2 n = PGFOptions
    { _surface    :: Surface       -- ^ Surface you want to use.
    , _sizeSpec   :: SizeSpec V2 n -- ^ The requested size.
    , _readable   :: Bool          -- ^ Indented lines for @.tex@ output.
    , _standalone :: Bool          -- ^ Should @.tex@ output be standalone.
    }

  renderRTree _ ops (toRender -> R r) =
    P.renderWith (ops^.surface) (ops^.readable) (ops^.standalone) bounds r
      where
        bounds = specToSize 100 (ops^.sizeSpec)

  adjustDia = adjustDia2D sizeSpec

toRender :: TypeableFloat n => RTree PGF V2 n Annotation -> Render PGF V2 n
toRender (Node n rs) = case n of
  RPrim p                 -> render PGF p
  RStyle sty'             -> R $ do
    sty <- P.style <<<>= sty' -- mappend old style
    doClip <- uses (P.style . _clip) null
    let scopeClip = if doClip then \cr -> P.scope $ (setClipPaths <~ op Clip) >> cr else id
    scopeClip r <* (P.style .= sty)     -- render then revert to old style
  RAnnot (OpacityGroup x) -> R $ P.opacityGroup x r
  _                       -> R r
  where
    R r = F.foldMap toRender rs

instance Fractional n => Default (Options PGF V2 n) where
  def = PGFOptions
          { _surface    = def
          , _sizeSpec   = absolute
          , _readable   = True
          , _standalone = False
          }

instance Monoid (Render PGF V2 n) where
  mempty              = R $ return ()
  R ra `mappend` R rb = R $ ra >> rb

-- | Lens onto the surface used to render.
surface :: Lens' (Options PGF V2 n) Surface
surface = lens _surface (\o s -> o {_surface = s})

-- | Lens onto whether a standalone TeX document should be produced.
standalone :: Lens' (Options PGF V2 n) Bool
standalone = lens _standalone (\o s -> o {_standalone = s})

-- | Lens onto the 'SizeSpec2D'.
sizeSpec :: Lens' (Options PGF V2 n) (SizeSpec V2 n)
sizeSpec = lens _sizeSpec (\o s -> o {_sizeSpec = s})

-- | Lens onto whether the lines of the TeX output are indented.
readable :: Lens' (Options PGF V2 n) Bool
readable = lens _readable (\o b -> o {_readable = b})

-- | Use the path that has already been drawn in scope. The path is stroked if
--   linewidth > 0.0001 and if filled if a colour is defined.
--
--   All stroke and fill properties from the current @style@ are also output here.
draw :: TypeableFloat n => Path V2 n -> P.Render n
draw path = P.scope $ do
  -- lines and loops are separated when stroking so we only need to
  -- check the first one
  let canFill = noneOf (_Wrapped' . _head . located) isLine path
  -- solid colours need to be filled at the end with usePath
  doFill <- if canFill
    then do
      mFillTexture <- preuse (P.style . _fillTexture)
      case mFillTexture of
        Nothing -> return False
        Just t  -> do
          setFillTexture path t
          P.setFillRule <~ getFillRule
          return (has _SC t)
    else return False
  --
  doStroke <- shouldStroke
  when doStroke $ do
    setLineTexture <~ getLineTexture -- stoke opacity needs to be set
    P.setLineJoin  <~ getLineJoin
    P.setLineWidth <~ getLineWidth
    P.setLineCap   <~ getLineCap
    P.setDash      <~ getDashing
  --
  P.path path
  P.usePath doFill doStroke

-- helper function to easily get options and set them
(<~) :: AttributeClass a => (b -> P.Render n) -> (a -> b) -> P.Render n
renderF <~ getF = do
  s <- uses P.style (fmap getF . getAttr)
  maybe (return ()) renderF s

infixr 2 <~

-- The Path is necessary so we can clip/workout gradients.
setFillTexture :: RealFloat n => Path V2 n -> Texture n -> P.Render n
setFillTexture p t = case t of
  SC (SomeColor c) -> setFillColor' c
  LG g             -> P.linearGradient p g
  RG g             -> P.radialGradient p g

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

-- | Queries the current style and decides if the path should be stroked. Paths
--   are stroked when lw > 0.0001
shouldStroke :: (OrderedField n, Typeable n) => P.RenderM n Bool
shouldStroke = do
  mLWidth <- getNumAttr getLineWidth <$> use P.style
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
      P.trail tr

renderPath :: TypeableFloat n => Path V2 n -> P.Render n
renderPath = draw

-- | Escapes some common characters in a string. Note that this does not
--   mean the string can't create an error, it mearly escapes common
--   characters.
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

-- | Renders text. Colour is set by fill colour. Opacity is inherited from
--   scope fill opacity. Does not support full alignment. Text is not escaped.
renderText :: (RealFloat n, Typeable n) => Text n -> P.Render n
renderText (Text tt txtAlign str) = P.scope $ do
  setFillTexture mempty <~ getFillTexture
  --
  P.applyTransform tt
  (P.applyScale . (/8)) <~ getFontSize
      -- (/8) was obtained from trail and error
  --
  P.renderText (P.setTextAlign txtAlign) $ do
    P.setFontWeight <~ getFontWeight
    P.setFontSlant  <~ getFontSlant
    P.rawString str

renderHbox :: RealFloat n => Hbox n -> P.Render n
renderHbox (Hbox tt str) = P.scope $ do
  P.applyTransform tt
  P.renderText (P.setTextAlign BaselineText) (P.rawString str)

------------------------------------------------------------------------
-- Renderable instances

instance TypeableFloat n => Renderable (Path V2 n) PGF where
  render _ = R . renderPath

instance TypeableFloat n => Renderable (Text n) PGF where
  render _ = R . renderText

instance TypeableFloat n => Renderable (Hbox n) PGF where
  render _ = R . renderHbox

-- | Supported: @.pdf@, @.jpg@, @.png@.
instance RealFloat n => Renderable (DImage n External) PGF where
  render _  = R . P.image

-- | Supported: 'ImageRGB8'. (Other types from 'DynamicImage' will
--   error)
instance RealFloat n => Renderable (DImage n Embedded) PGF where
  render _  = R . P.embeddedImage

------------------------------------------------------------------------
-- Hashable instances

instance Hashable n => Hashable (Options PGF V2 n) where
  hashWithSalt s (PGFOptions sf sz rd st)
    = s  `hashWithSalt`
      sf `hashWithSalt`
      sz `hashWithSalt`
      rd `hashWithSalt`
      st

