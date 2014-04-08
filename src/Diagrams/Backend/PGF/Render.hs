{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
module Diagrams.Backend.PGF.Render
  ( PGF (..)
  , Options (..)
  -- * Lenses
  , template
  , surface
  , sizeSpec
  , sizeSpecToBounds
  -- , preserveLineWidth
  , readable
  , standalone
  ) where

import           Control.Lens              (lens, (.=), (%=), (^.), op,
                                            Lens', use)
import           Control.Monad             (when, unless)
import           Data.Default
import           Data.Foldable             (foldMap)
import           Data.Hashable             (Hashable (..))
import           Data.Maybe                (isJust)
import           Data.Typeable
import           Diagrams.Core.Compile
import           Diagrams.Core.Types       (Annotation) --, Renderable (..), 
                                            -- Backend (..))
import           Diagrams.Prelude
import           Diagrams.TwoD.Adjust      (adjustDia2D)
import           Diagrams.TwoD.Path
import           Diagrams.TwoD.Text
import           Diagrams.TwoD.Types       (r2)
import           Diagrams.TwoD.Typeset
import           Data.Tree
import qualified Blaze.ByteString.Builder  as Blaze

import qualified Graphics.Rendering.PGF        as P
import           Diagrams.Backend.PGF.Surface
import           Diagrams.Backend.PGF.Hbox

-- | This data declaration is simply used as a token to distinguish
--   this rendering engine.
data PGF = PGF
  deriving (Show, Typeable)


instance Backend PGF R2 where
  data Render  PGF R2 = P (P.Render)
  type Result  PGF R2 = Blaze.Builder
  data Options PGF R2 = PGFOptions
      { _surface    :: Surface    -- ^ Surface you want to use.
      , _sizeSpec   :: SizeSpec2D -- ^ The requested size.
      , _readable   :: Bool       -- ^ Pretty print output.
      , _standalone :: Bool       -- ^ Should .tex output be standalone 
                                  --   ('renderPDF' sets this to true)
      }

  renderRTree _ ops rt =
    P.renderWith (ops^.surface) (ops^.readable) (ops^.standalone) bounds r
    where
      (P r) = toRender rt
      bounds = sizeSpecToBounds (ops^.sizeSpec)

  adjustDia = adjustDia2D sizeSpec

  -- withStyle _ s t (P r) = P . P.scope $ do
  --   P.applyTransform t
  --   P.style %= (<> s)
  --   setClipPaths <~ op Clip
  --   r


  -- doRender _ ops (P r) =
  --   P.renderWith (ops^.surface) (ops^.readable) (ops^.standalone) bounds r
  --   where bounds = sizeSpecToBounds (ops^.sizeSpec)
  -- 
  -- adjustDia =
  --     adjustDiaSize2D (view sizeSpec) (set sizeSpec)

toRender :: RTree PGF R2 Annotation -> Render PGF R2
toRender = fromRTree
  -- . Node (RStyle (mempty # recommendFillColor (transparent :: AlphaColour Double)))
  -- . (:[])
  -- . splitFills
    where
      -- fromRTree (Node (RAnnot (Href uri)) rs)
      --   = R $ do
      --       let R r =  foldMap fromRTree rs
      --       svg <- r
      --       return $ (S.a ! xlinkHref (S.toValue uri)) svg
      fromRTree (Node (RPrim p) _)     = render PGF p
      fromRTree (Node (RStyle sty) rs) = P . P.scope $ do
            let P r = foldMap fromRTree rs
            P.style %= (<> sty)
            setClipPaths <~ op Clip
            pgf <- r
            P.resetState
            return pgf
      fromRTree (Node _ rs)            = foldMap fromRTree rs


sizeSpecToBounds :: SizeSpec2D -> (Double, Double)
sizeSpecToBounds spec = case spec of
   Width w  -> (w,w)
   Height h -> (h,h)
   Dims w h -> (w,h)
   Absolute -> (100,100)

instance Default (Options PGF R2) where
  def = PGFOptions
          { _surface    = def
          , _sizeSpec   = Absolute
          , _readable   = True
          , _standalone = False
          }

-- | Lens to change the template, aka surface defined in Diagrams.Backend.PGF.Surface
template :: Lens' (Options PGF R2) Surface
template = lens getter setter
  where
    getter (PGFOptions { _surface = s }) = s
    setter o s = o { _surface = s }

standalone :: Lens' (Options PGF R2) Bool
standalone = lens getter setter
  where
    getter (PGFOptions { _standalone = s }) = s
    setter o s = o { _standalone = s }

surface :: Lens' (Options PGF R2) Surface
surface = template

sizeSpec :: Lens' (Options PGF R2) SizeSpec2D
sizeSpec = lens getSize setSize
  where getSize (PGFOptions { _sizeSpec = s }) = s
        setSize o s = o { _sizeSpec = s }

-- | Pretty print the output with indented lines, default is true.
readable :: Lens' (Options PGF R2) Bool
readable = lens getR setR
  where
    getR (PGFOptions { _readable = r }) = r
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

-- instance Hashable (Options Cairo R2)

instance Monoid (Render PGF R2) where
  mempty  = P $ return ()
  (P ra) `mappend` (P rb) = P (ra >> rb)

renderP :: (Renderable a PGF, V a ~ R2) => a -> P.RenderM ()
renderP (render PGF -> P r) = r
  
-- | Use the path that has already been drawn in scope. The path is stroked if 
--   linewidth > 0.0001 and if filled if a colour is defined.
--
--   All stroke and fill properties from the cuuent @style@ are also output here.
draw :: P.RenderM ()
draw = do
  doFill <- shouldFill
  when doFill $ do
    setFillColor' <~ getFillColor
    P.setFillRule <~ getFillRule
  --
  doStroke <- shouldStroke
  when doStroke $ do
    setLineColor'  <~ getLineColor -- stoke opacity needs to be set
    P.setLineJoin  <~ getLineJoin
    P.setLineWidth <~ fromOutput . getLineWidth
    P.setLineCap   <~ getLineCap
    P.setDash      <~ getDashing
  -- 
  P.usePath doFill doStroke False

-- helper function to easily get options and set them
(<~) :: (AttributeClass a) => (b -> P.RenderM ()) -> (a -> b) -> P.RenderM ()
renderF <~ getF = do
  s <- use P.style
  let mAttr = (getF <$>) . getAttr $ s
  maybe (return ()) renderF mAttr

infixr 2 <~

setFillColor' :: (Color c) => c -> P.RenderM ()
setFillColor' c = do
  s <- use P.style
  P.setFillColor $ applyOpacity c s

setLineColor' :: (Color c) => c -> P.RenderM ()
setLineColor' c = do
  s <- use P.style
  P.setLineColor $ applyOpacity c s

-- | Apply the opacity from a style to a given color.
applyOpacity :: Color c => c -> Style v -> AlphaColour Double
applyOpacity c s = dissolve (maybe 1 getOpacity (getAttr s)) (toAlphaColour c)

-- | Queries the current style and decides if the path should be filled. Paths 
--   are filled if a color is defined
shouldFill :: P.RenderM Bool
shouldFill = do
  fColor <- (getFillColor <$>) . getAttr <$> use P.style
  ignore <- use P.ignoreFill
  --
  return $ not ignore && isJust fColor

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
    renderTrail (viewLoc -> (unp2 -> p, tr)) = do
      P.moveTo (r2 p)
      renderP tr

renderPath :: Path R2 -> P.Render
renderPath (Path trs) = do
  when (any (isLine . unLoc) trs) $ P.ignoreFill .= True
  mapM_ renderTrail trs
  draw
  where
    renderTrail (viewLoc -> (unp2 -> p, tr)) = do
      P.moveTo (r2 p)
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
renderText (Text tr txtAlign str) = do
  setFillColor' <~ getFillColor
  --
  -- doTxtTrans <- view P.txtTrans
  P.applyTransform tr
  -- if doTxtTrans
  (P.applyScale . (/8)) <~ fromOutput . getFontSize
      -- (/8) was obtained from trail and error
    -- else P.resetNonTranslations
  --
  P.renderText (P.setTextAlign txtAlign) $ do
    P.setFontWeight <~ getFontWeight
    P.setFontSlant  <~ getFontSlant
    P.rawString $ escapeString str

renderTypeset :: Typeset -> P.Render
renderTypeset (Typeset str tpsSize angle tpsAlign tr) = do
  setFillColor' <~ getFillColor
  P.applyTransform tr
  --
  let isDiagramSize = case tpsSize of
                        DiagramsSize _ -> True
                        _              -> False
  if isDiagramSize
    then P.typesetSize tpsSize
    else P.resetNonTranslations

  --
  let ops = P.setTextAlign tpsAlign
         ++ P.setTextRotation angle
  P.renderText ops $ do
    P.setFontWeight <~ getFontWeight
    P.setFontSlant  <~ getFontSlant
    unless isDiagramSize $ P.typesetSize tpsSize
    P.rawString str

renderRaw :: Hbox -> P.Render
renderRaw (Hbox tr str) = do
  P.applyTransform tr
  P.resetNonTranslations
  P.renderText [] (P.rawString str)

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

instance Renderable Typeset PGF where
  render _ = P . renderTypeset

instance Renderable Hbox PGF where
  render _ = P . renderRaw

instance Renderable Image PGF where
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
