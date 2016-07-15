{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE NoMonomorphismRestriction    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.PGF
-- Copyright   :  (c) 2015 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This is an internal module exposeing internals for rendering a
-- diagram. This is for advanced use only. 'Diagrams.Backend.PGF'
-- has enought for general use.
--
module Diagrams.Backend.PGF
  ( PGF (..)
  , Options (..)

  , savePGF
  , savePGFSurf
  , saveOnlinePGF
  , saveOnlinePGF'

  -- * Lenses
  , surface
  , sizeSpec
  , readable
  , standalone

  -- * Utilities
  , escapeString
  ) where

import           Control.Monad                (when)
import Control.Monad.Reader (local)
import           Data.ByteString.Builder
import           Data.Hashable                (Hashable (..))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import System.FilePath (FilePath, takeExtension, takeDirectory)
import System.Directory (getCurrentDirectory, canonicalizePath)
import Data.Maybe (fromMaybe)
import qualified Options.Applicative          as OP
import           Data.Typeable

import           Diagrams.TwoD.Path
import           Diagrams.Backend
import           Diagrams.Attributes
import           Diagrams.Prelude hiding ((<~))
import           Diagrams.Style
import           Diagrams.TwoD.Attributes
import Diagrams.Backend.Compile
import Diagrams.TwoD.Text

import Diagrams.Types
import Geometry.Space
import Geometry.TwoD.Types
import Geometry.Path.Unboxed
-- import Geometry.Trail.Unboxed

import           System.Texrunner.Online      hiding (hbox)
import           System.Texrunner (runTex, prettyPrintLog)

import           Diagrams.Backend.PGF.Surface
import           Diagrams.Backend.PGF.Hbox
import qualified Graphics.Rendering.PGF       as P
import           Graphics.Rendering.PGF       (Render)

------------------------------------------------------------------------
-- The PGF backend
------------------------------------------------------------------------

-- | This data declaration is simply used as a token to distinguish
--   this rendering engine.
data PGF = PGF
  deriving (Show, Typeable)

type instance V PGF = V2
type instance N PGF = Double

default2DAttrs :: Diagram V2 -> Diagram V2
default2DAttrs
  = lineWidth medium
  . lineTexture black
  -- .  lineCap def
  --     # lineJoin def
  --     # lineMiterLimitA def

instance Backend PGF where
  type Result  PGF = Builder
  data Options PGF = PGFOptions
    { _surface     :: Surface         -- ^ Surface you want to use.
    , _pgfSizeSpec :: SizeSpec V2 Int -- ^ The requested size.
    , _readable    :: Bool            -- ^ Indented lines for @.tex@ output.
    , _standalone  :: Bool            -- ^ Should @.tex@ output be standalone.
    }

  renderDiaT opts dia = (b, t2) where
    (sz, t2, dia') = adjustSize2D (opts^.sizeSpec) (default2DAttrs dia)
    b = P.renderWith (opts^.surface) (opts^.readable) (opts^.standalone) sz r
    r = toRender t2 dia'

  -- renderRTree _ ops (toRender -> R r) =
  --   P.renderWith (ops^.surface) (ops^.readable) (ops^.standalone) bounds r
  --     where
  --       bounds = specToSize 100 (ops^.sizeSpec)

  -- adjustDia = adjustDia2D sizeSpec

instance Parseable (Options PGF) where
  parser = PGFOptions <$> parser <*> parser <*> readParser <*> standaloneParser
    where
      standaloneParser = OP.switch $ mconcat
        [ OP.long "standalone", OP.short 'a'
        , OP.help "Produce standalone .tex output (no effect on .pdf output)"
        ]
      readParser = OP.switch $ mconcat
        [ OP.long "readable", OP.short 'r'
        , OP.help "Indent lines for .tex (no effect on .pdf output)"
        ]

instance BackendBuild PGF where
  saveDiagram' opts outPath d =
    case takeExtension outPath of
      ".pdf" -> do
        let opts'    = opts & standalone .~ True
                            & readable   .~ False
        let rendered = fst $ renderDiaT opts' d

        currentDir <- getCurrentDirectory
        targetDir  <- canonicalizePath (takeDirectory outPath)

        let source = toLazyByteString rendered

        (_, texLog, mPDF) <- runTex (opts^.surface.command)
                                    (opts^.surface.arguments)
                                    [currentDir, targetDir]
                                    source

        case mPDF of
          Nothing  -> putStrLn "Error, no PDF found:"
                   >> B.putStrLn (prettyPrintLog texLog)
          Just pdf -> LB.writeFile outPath pdf

      -- if it's not a pdf, just output raw tex
      _      -> writeTexFile opts outPath d

  mkOptions sz = def & sizeSpec .~ sz
  sizeSpec     = pgfSizeSpec

instance Default (Options PGF) where
  def = PGFOptions
    { _surface     = def
    , _pgfSizeSpec = absolute
    , _readable    = True
    , _standalone  = False
    }

-- | Lens onto the surface used to render.
surface :: Lens' (Options PGF) Surface
surface = lens _surface (\o s -> o {_surface = s})

-- | Lens onto whether a standalone TeX document should be produced.
standalone :: Lens' (Options PGF) Bool
standalone = lens _standalone (\o s -> o {_standalone = s})

-- | Lens onto the 'SizeSpec' for pgf options.
pgfSizeSpec :: Lens' (Options PGF) (SizeSpec V2 Int)
pgfSizeSpec = lens _pgfSizeSpec (\o s -> o {_pgfSizeSpec = s})

-- | Lens onto whether the lines of the TeX output are indented.
readable :: Lens' (Options PGF) Bool
readable = lens _readable (\o b -> o {_readable = b})

------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------

toRender :: T2 Double -> Diagram V2 -> Render Double
toRender = foldDia' renderPrim renderAnnot renderSty
  where
    renderPrim t2 attrs prim = case renderPrimitive t2 attrs prim of
      Just r  -> local (P.attributes .~ attrs) r
      Nothing -> error $ "Unknown primitive"

    renderAnnot (OpacityGroup x) r = P.opacityGroup x r
    renderAnnot _ r                = r

    -- To avoid repetition, we apply the clip higher up in the
    renderSty attrs r = clip (getAttr _Clip attrs ^. non' _Empty) r

renderPrimitive
  :: T2 Double -> Attributes -> Prim V2 Double -> Maybe (Render Double)
renderPrimitive t2 attrs prim -- (Prim p) = case pr
  | Just p <- preview _UPath prim = Just $ renderUPath t2 attrs p
  | Just p <- preview _Path  prim = Just $ renderPath t2 attrs p
  | Just p <- preview _Text  prim = Just $ renderText t2 attrs p
  | Just p <- preview _Hbox  prim = Just $ renderHbox t2 attrs p
  | otherwise                     = Nothing

-- toRender :: TypeableFloat n => RTree PGF V2 n Annotation -> Render PGF V2 n
-- toRender (Node n rs) = case n of
--   RPrim p                 -> render PGF p
--   RStyle sty'             -> R $ do
--     sty <- P.style <<<>= sty'        -- mappend old style
--     clips <- use (P.style . _clip)
--     clip clips r <* (P.style .= sty) -- render then revert to old style
--   RAnnot (OpacityGroup x) -> R $ P.opacityGroup x r
--   _                       -> R r
--   where R r = F.foldMap toRender rs


-- helper function to easily get options and set them
-- (<~) :: AttributeClass a => (b -> P.Render n) -> (a -> b) -> P.Render n
(<~) :: AttributeClass a => (r -> P.Render Double) -> Getting r a r -> P.Render Double
renderF <~ getter = do
  s <- views P.attributes (getAttr getter)
  maybe (return ()) renderF s

infixr 2 <~

-- | Fade a colour with the opacity from the style.
fade :: (Color c, Typeable a)
     => Getting Double a Double
     -> c
     -> P.RenderM n (AlphaColour Double)
fade g c = view P.attributes <&> \s ->
  -- dissolve (productOf (_Opacity <> g) s) (toAlphaColour c)
  let getO1 = fromMaybe 1 . flip getAttr s
      getO2 = fromMaybe 1 . flip getAttr s
  in  dissolve (getO1 _Opacity * getO2 g) (toAlphaColour c)

-- | The Path is necessary so we can clip/workout gradients.
--   (Should use envelope for this)
setFillTexture :: Path V2 Double -> Texture -> P.Render Double
setFillTexture p t = case t of
  SC (SomeColor c) -> fade _FillOpacity c >>= P.setFillColor
  LG g             -> P.linearGradient p g
  RG g             -> P.radialGradient p g

setLineTexture :: Texture -> P.Render Double
setLineTexture (SC (SomeColor c)) = fade _StrokeOpacity c >>= P.setLineColor
setLineTexture _                  = return ()

clip :: TypeableFloat n => [Path V2 n] -> P.Render n -> P.Render n
clip paths r = go paths where
  go []     = r
  go (p:ps) = P.scope $ P.path p >> P.clip >> go ps

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

-- Renderable instances ------------------------------------------------

renderPath :: T2 Double -> Attributes -> Path V2 Double -> Render Double
renderPath t2 attrs path = P.scope $ do
  -- lines and loops are separated when stroking so we only need to
  -- check the first one
  let canFill = noneOf (_head . located) isLine path

  -- solid colours need to be filled with usePath
  doFill <- if canFill
    then do
      mFillTexture <- views P.attributes (getAttr _FillTexture)
      case mFillTexture of
        Nothing -> return False
        Just t  -> do
          setFillTexture path t
          P.setFillRule <~ _FillRule
          return (has _SC t)
    else return False

  let w = fromMaybe 0 $ getAttr _LineWidth attrs
  let doStroke = w > 0.0001
  when doStroke $ do
    P.setLineWidth w
    setLineTexture <~ _LineTexture
    P.setLineJoin  <~ _LineJoin
    P.setLineCap   <~ _LineCap
    P.setDash      <~ _Dashing
  --
  P.path (transform t2 path)
  P.usePath doFill doStroke

renderUPath :: T2 Double -> Attributes -> UPath V2 Double -> Render Double
renderUPath t2 attrs path = P.scope $ do
  -- lines and loops are separated when stroking so we only need to
  -- check the first one
  --
  -- I'm not convinced this is right. Separating the paths when we
  -- stroke messses up the order of the paths
  let canFill = True -- noneOf (_head . located) isLine path

  -- solid colours need to be filled with usePath
  doFill <- if canFill
    then do
      case getAttr _FillTexture attrs of
        Nothing -> return False
        Just t  -> do
          setFillTexture undefined t -- path t
          P.setFillRule <~ _FillRule
          return (has _SC t)
    else return False

  let w = fromMaybe 0 $ getAttr _LineWidth attrs
  -- w <- use (P.attributes . _lineWidth . non 0)
  let doStroke = w > 0.0001
  when doStroke $ do
    P.setLineWidth w
    -- setLineTexture <~ _LineTexture
    P.setLineJoin  <~ _LineJoin
    P.setLineCap   <~ _LineCap
    P.setDash      <~ _Dashing
  --
  P.uPath t2 path
  P.usePath doFill doStroke


-- instance TypeableFloat n => Renderable (Path V2 n) PGF where
--   render _ path = R . P.scope $ do
--     -- lines and loops are separated when stroking so we only need to
--     -- check the first one
--     let canFill = noneOf (_head . located) isLine path
--     -- solid colours need to be filled with usePath
--     doFill <- if canFill
--       then do
--         mFillTexture <- preuse (P.style . _fillTexture)
--         case mFillTexture of
--           Nothing -> return False
--           Just t  -> do
--             setFillTexture path t
--             P.setFillRule <~ getFillRule
--             return (has _SC t)
--       else return False
--     --
--     w <- use (P.style . _lineWidthU . non 0)
--     let doStroke = w > 0.0001
--     when doStroke $ do
--       P.setLineWidth w
--       setLineTexture <~ getLineTexture
--       P.setLineJoin  <~ getLineJoin
--       P.setLineCap   <~ getLineCap
--       P.setDash      <~ getDashing
--     --
--     P.path path
--     P.usePath doFill doStroke

-- | Does not support full alignment. Text is not escaped.
-- instance TypeableFloat n => Renderable (Text n) PGF where
renderText :: T2 Double -> Attributes -> Text Double -> Render Double
renderText t2 _attrs (Text txtAlign str) = P.scope $ do
  setFillTexture mempty <~ _FillTexture
  --
  P.applyTransform t2
  (P.applyScale . (/8)) <~ _FontSize
      -- (/8) was obtained from trail and error
  --
  P.renderText (P.setTextAlign txtAlign) $ do
    P.setFontWeight <~ _FontWeight
    P.setFontSlant  <~ _FontSlant
    P.rawString str

renderHbox :: T2 Double -> Attributes -> Hbox Double -> Render Double
renderHbox t2 _attrs (Hbox str) = P.scope $ do
  setFillTexture mempty <~ _FillTexture
  P.applyTransform t2
  P.renderText (P.setTextAlign BaselineText) (P.rawString str)

-- -- | Supported: @.pdf@, @.jpg@, @.png@.
-- instance RealFloat n => Renderable (DImage n External) PGF where
--   render _  = R . P.image

-- -- | Supported: 'ImageRGB8'. (Other types from 'DynamicImage' will
-- --   error)
-- instance RealFloat n => Renderable (DImage n Embedded) PGF where
--   render _  = R . P.embeddedImage

------------------------------------------------------------------------
-- Hashable instances

instance Hashable (Options PGF) where
  hashWithSalt s (PGFOptions sf _sz rd st)
    = s  `hashWithSalt`
      sf `hashWithSalt`
      -- sz `hashWithSalt`
      rd `hashWithSalt`
      st

------------------------------------------------------------------------
-- Saving to file
------------------------------------------------------------------------

-- | Render a pgf diagram and write it to the given filepath. Same as
--   'renderPGF'' but uses the default options.
savePGF
  :: SizeSpec V2 Int  -- ^ size of output
  -> FilePath         -- ^ path to output
  -> Diagram V2       -- ^ 'Diagram' to render
  -> IO ()
savePGF = saveDiagram PGF

-- | Render a pgf diagram and write it to the given filepath. Same as
--   'renderPGF' but takes a 'Surface'.
savePGFSurf
  :: SizeSpec V2 Int    -- ^ size of output
  -> Surface          -- ^ surface to render with
  -> FilePath         -- ^ path to output
  -> Diagram V2 -- ^ diagram to render
  -> IO ()
savePGFSurf sz surf =
  saveDiagram' $ mkOptions sz & surface .~ surf

-- | Render a pgf diagram and write it to the given filepath. If the file has
--   the extension @.pdf@, a PDF is generated in a temporary directory using
--   options from the given surface, otherwise, the tex output is saved
--   using the surface's 'TexFormat'.

-- | Render an online 'PGF' diagram and save it. Same as
--   'renderOnlinePGF'' using default options.
saveOnlinePGF
  :: SizeSpec V2 Int
  -> FilePath
  -> OnlineTex (Diagram V2)
  -> IO ()
saveOnlinePGF sz = saveOnlinePGF' (mkOptions sz)

-- | Same as 'renderOnlinePDF' but takes 'Options' 'PGF'.
saveOnlinePGF'
  :: Options PGF
  -> FilePath
  -> OnlineTex (Diagram V2)
  -> IO ()
saveOnlinePGF' opts outPath dOL = case takeExtension outPath of
  ".pdf" -> do

    ((), texLog, mPDF) <-
      runOnlineTex'
        (surf^.command)
        (surf^.arguments)
        (toByteString . stringUtf8 $ surf ^. (preamble <> beginDoc)) $ do

          d <- dOL

          -- we've already output the preamble so don't do it again
          let opts' = opts & surface    %~ set beginDoc "" . set preamble ""
                           & readable   .~ False
                           & standalone .~ True

              rendered = fst $ renderDiaT opts' d

          texPutStrLn $ toByteString rendered

    case mPDF of
      Nothing  -> putStrLn "Error, no PDF found:"
               >> print texLog
      Just pdf -> LB.writeFile outPath pdf

  -- tex output
  _      ->  surfOnlineTexIO surf dOL >>= writeTexFile opts outPath
  where
    surf = opts ^. surface
    toByteString = LB.toStrict . toLazyByteString

-- | Write the rendered diagram to a text file, ignoring the extension.
writeTexFile
  :: Options PGF
  -> FilePath
  -> Diagram V2
  -> IO ()
writeTexFile opts outPath d = do
  let bs = toLazyByteString . fst $ renderDiaT opts d
  LB.writeFile outPath bs
  -- h <- openFile outPath WriteMode
  -- hSetBinaryMode h True
  -- hSetBuffering h (BlockBuffering (Just 80000))
  -- hPutBuilder h $ renderDia PGF opts d
  -- hClose h


