{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.PGF
-- Copyright   :  (c) 2015-2016 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Render diagrams using PGF, a TeX macro package for generating
-- graphics.
--
module Diagrams.Backend.PGF
  ( PGF (..)
  , Options (..)

  -- * Command line
  , mainWithPGF
  , mainWithSurface

  -- * Saving to files
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

  -- * Reexports
  , OnlineTex
  , module Diagrams.Backend.PGF.Hbox
  ) where

import           Control.Monad                (when)
import           Control.Monad.Reader         (local)
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy         as LB
import qualified Data.Foldable                as F
import           Data.Hashable                (Hashable (..))
import           Data.Maybe                   (fromMaybe)
import           Data.Typeable
import qualified Options.Applicative          as OP
import           System.Directory             (canonicalizePath,
                                               getCurrentDirectory)
import           System.FilePath              (FilePath, takeDirectory,
                                               takeExtension)

import           Diagrams.Backend.Compile
import           Diagrams.Prelude             hiding (clip, local, (<~))
import           Diagrams.TwoD.Text
import           Diagrams.Types               hiding (local)

import           System.Texrunner             (prettyPrintLog, runTex)
import           System.Texrunner.Online      hiding (hbox)

import           Diagrams.Backend.PGF.Hbox
import           Diagrams.Backend.PGF.Surface
import           Graphics.Rendering.PGF       (Render)
import qualified Graphics.Rendering.PGF       as P

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

  renderDiaT opts dia = (sz, t2, b) where
    (sz, t2, dia') = adjustSize2D (opts^.sizeSpec) (default2DAttrs dia)
    b = P.renderWith (opts^.surface) (opts^.readable) (opts^.standalone) sz r
    r = toRender t2 dia'

  backendInfo _ = pgfInfo

  -- renderRTree _ ops (toRender -> R r) =
  --   P.renderWith (ops^.surface) (ops^.readable) (ops^.standalone) bounds r
  --     where
  --       bounds = specToSize 100 (ops^.sizeSpec)

  -- adjustDia = adjustDia2D sizeSpec

deriving instance Show (Options PGF)

optionsParser :: OP.Parser (Options PGF)
optionsParser = PGFOptions <$> surfaceParser <*> sizeParser <*> readParser <*> standaloneParser
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
  saveDiagram' outPath opts d =
    case takeExtension outPath of
      ".pdf" -> do
        let opts'    = opts & standalone .~ True
                            & readable   .~ False
        let rendered = view _3 $ renderDiaT opts' d

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
  showOptions  = show

-- It's an ugly type signature but it seems to get the job done. Is
-- there a nicer way to this?
-- I could have a PGFMainOpts data type instead of (FilePath, Options
-- PGF). This would make it slightly better
--
-- This could be a bit nicer if MainOpts was separate type family only
-- on the backend. Are there any backends where this could be a problem?

-- | Class of things whose 'Outcome' can be rendered by the PGF backend.
--   This includes
--
-- @
-- MainableWithPGF (Diagram V2)
-- MainableWithPGF (OnlineTex (Diagram V2))
-- @
--
--   But also includes all the other 'Mainable' magic
--
-- @
-- MainableWithPGF (FilePath -> Colour Double -> IO ([String, Diagram V2]))
-- @
class (MainWith PGF a, MainOpts PGF (Outcome a) ~ (FilePath, Options PGF)) => MainableWithPGF a
instance (MainWith PGF a, MainOpts PGF (Outcome a) ~ (FilePath, Options PGF)) => MainableWithPGF a

-- | 'mainWith' 'PGF'
mainWithPGF :: MainableWithPGF a => a -> IO ()
mainWithPGF = mainWith PGF

-- @
-- mainWithSurface :: Surface -> Diagram V2             -> IO ()
-- mainWithSurface :: Surface -> OnlineTex (Diagram V2) -> IO ()
-- @
mainWithSurface :: MainableWithPGF a => Surface -> a -> IO ()
mainWithSurface surf a = do
  let parse = (,,,) <$> argsParser (Identity a) <*> outputParser <*> standaloneParser <*> readParser
      standaloneParser = OP.switch $ mconcat
        [ OP.long "standalone", OP.short 'a'
        , OP.help "Produce standalone .tex output (no effect on .pdf output)"
        ]
      readParser = OP.switch $ mconcat
        [ OP.long "readable", OP.short 'r'
        , OP.help "Indent lines for .tex (no effect on .pdf output)"
        ]
  (args, path, isStandalone, isReadable) <- defaultExecParser parse
  let opts = def & surface    .~ surf
                 & standalone .~ isStandalone
                 & readable   .~ isReadable
  withOutcome (renderOutcome PGF (path, opts)) args a

instance RenderOutcome PGF (Diagram V2) where
  type MainOpts PGF (Diagram V2) = (FilePath, Options PGF)

  resultParser _ _ = (,) <$> outputParser <*> optionsParser
  renderOutcome _ (path, opts) = saveDiagram' path opts

instance RenderOutcome PGF (OnlineTex (Diagram V2)) where
  type MainOpts PGF (OnlineTex (Diagram V2)) = (FilePath, Options PGF)
  resultParser _ _ = (,) <$> outputParser <*> optionsParser
  renderOutcome _ (path, opts) = saveOnlinePGF' path opts

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
toRender = foldDiaA renderPrim renderAnnot
  where
    renderPrim t2 attrs prim = case renderPrimitive t2 attrs prim of
      Just r  -> local (P.attributes .~ attrs) r
      Nothing -> error $ "Unknown primitive"

renderPrimitive
  :: T2 Double -> Attributes -> Prim V2 Double -> Maybe (Render Double)
renderPrimitive t2 attrs = \case
  Path_ path -> Just $ renderPath t2 attrs path
  Text_ t     -> Just $ renderText t2 attrs t
  Hbox_ str   -> Just $ renderHbox t2 attrs str
  Prim _      -> Nothing

renderAnnot :: Annotation V2 Double -> Render Double -> Render Double
renderAnnot a
  | Just x <- getAnnot _GroupOpacity a = P.opacityGroup x
  | Just p <- getAnnot _Clip         a = clip (F.toList p)
  | otherwise                          = id

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
setFillTexture :: Envelope V2 Double -> Texture -> P.Render Double
setFillTexture _e t = case t of
  SC (SomeColor c) -> fade _FillOpacity c >>= P.setFillColor
  _ -> error "gradients not implimented yet"
  -- LG g             -> P.linearGradient p g
  -- RG g             -> P.radialGradient p g

setLineTexture :: Texture -> P.Render Double
setLineTexture (SC (SomeColor c)) = fade _StrokeOpacity c >>= P.setLineColor
setLineTexture _                  = return ()

clip :: [Path V2 Double] -> P.Render Double -> P.Render Double
clip paths r = go paths where
  go []     = r
  go (p:ps) = P.scope $ P.path mempty p >> P.clip >> go ps

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

-- renderPath :: T2 Double -> Attributes -> Path V2 Double -> Render Double
-- renderPath t2 attrs path = P.scope $ do
--   -- lines and loops are separated when stroking so we only need to
--   -- check the first one
--   let canFill = noneOf (_head . located) isLine path

--   -- solid colours need to be filled with usePath
--   doFill <- if canFill
--     then do
--       mFillTexture <- views P.attributes (getAttr _FillTexture)
--       case mFillTexture of
--         Nothing -> return False
--         Just t  -> do
--           setFillTexture path t
--           P.setFillRule <~ _FillRule
--           return (has _SC t)
--     else return False

--   let w = fromMaybe 0 $ getAttr _LineWidth attrs
--   let doStroke = w > 0.0001
--   when doStroke $ do
--     P.setLineWidth w
--     setLineTexture <~ _LineTexture
--     P.setLineJoin  <~ _LineJoin
--     P.setLineCap   <~ _LineCap
--     P.setDash      <~ _Dashing
--   --
--   P.path (transform t2 path)
--   P.usePath doFill doStroke

renderPath :: T2 Double -> Attributes -> Path V2 Double -> Render Double
renderPath t2 attrs path = P.scope $ do
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
          setFillTexture mempty t -- path t
          P.setFillRule <~ _FillRule
          return (has _SC t)
    else return False

  let w = fromMaybe 0 $ getAttr _LineWidth attrs
  -- w <- use (P.attributes . _lineWidth . non 0)
  let doStroke = w > 0.0001
  when doStroke $ do
    P.setLineWidth w
    setLineTexture <~ _LineTexture
    P.setLineJoin  <~ _LineJoin
    P.setLineCap   <~ _LineCap
    P.setDash      <~ _Dashing
  --
  P.path t2 path
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

renderHbox :: T2 Double -> Attributes -> String -> Render Double
renderHbox t2 _attrs str = P.scope $ do
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
  :: FilePath         -- ^ path to output
  -> SizeSpec V2 Int  -- ^ size of output
  -> Diagram V2       -- ^ 'Diagram' to render
  -> IO ()
savePGF = saveDiagram PGF

-- | Render a pgf diagram and write it to the given filepath. Same as
--   'renderPGF' but takes a 'Surface'.
savePGFSurf
  :: FilePath         -- ^ path to output
  -> SizeSpec V2 Int    -- ^ size of output
  -> Surface          -- ^ surface to render with
  -> Diagram V2 -- ^ diagram to render
  -> IO ()
savePGFSurf path sz surf =
  saveDiagram' path $ mkOptions sz & surface .~ surf

-- | Render a pgf diagram and write it to the given filepath. If the file has
--   the extension @.pdf@, a PDF is generated in a temporary directory using
--   options from the given surface, otherwise, the tex output is saved
--   using the surface's 'TexFormat'.

-- | Render an online 'PGF' diagram and save it. Same as
--   'renderOnlinePGF'' using default options.
saveOnlinePGF
  :: FilePath
  -> SizeSpec V2 Int
  -> OnlineTex (Diagram V2)
  -> IO ()
saveOnlinePGF path sz = saveOnlinePGF' path (mkOptions sz)

-- | Same as 'renderOnlinePDF' but takes 'Options' 'PGF'.
saveOnlinePGF'
  :: FilePath
  -> Options PGF
  -> OnlineTex (Diagram V2)
  -> IO ()
saveOnlinePGF' outPath opts dOL = case takeExtension outPath of
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

              rendered = view _3 $ renderDiaT opts' d

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
  let bs = toLazyByteString . view _3 $ renderDiaT opts d
  LB.writeFile outPath bs
  -- h <- openFile outPath WriteMode
  -- hSetBinaryMode h True
  -- hSetBuffering h (BlockBuffering (Just 80000))
  -- hPutBuilder h $ renderDia PGF opts d
  -- hClose h


