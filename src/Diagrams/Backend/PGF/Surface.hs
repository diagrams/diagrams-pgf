{-# LANGUAGE TemplateHaskell #-}
module Diagrams.Backend.PGF.Surface
    ( -- * Surface definition
      Surface(..)
    , TeXFormat(..)
    , texFormat
    , textTransforms
    , content
    , rectangleBoundingBox
      -- * Predefined surfaces
    , latexSurface
    , latexStandaloneSurface
    , latexPreviewSurface
    , previewSurface
    , contextSurface
    , contextStandaloneSurface
    , plaintexSurface
    , plaintexStandaloneSurface
    ) where

import Control.Lens
import Data.Default
import Text.Printf

data TeXFormat = LaTeX | ConTeXt | PlainTeX
  deriving (Show, Read, Eq)

-- | A surface contains the tex format and a functions that takes the diagram 
--   bounds in pixels and gives the appropriate header and footer. 
--   Implimentation likely to change.
data Surface = Surface
  { _texFormat :: TeXFormat
  , _textTransforms :: Bool
  , _content :: (Double,Double) -> (String, String)
  }

makeLenses ''Surface

instance Show Surface where
  show s = previewSurface s (200,300)

previewSurface :: Surface -> (Double, Double) -> String
previewSurface (Surface format _ c) bounds = concat
  [ "<<" ++ show format ++ ">>\n"
  , h
  , "    <<diagram>>\n"
  , f
  ]
  where
    (h,f) = c bounds

instance Default Surface where
    def = latexSurface

------------------------------------------------------------
-- Predefined surfaces

pdfPageBounds :: (Double,Double) -> (Double, Double) -> String
pdfPageBounds (x,y) (x0,y0) = unlines
  [ printf "\\pdfpagewidth=%.4f px" x
  , printf "\\pdfpageheight=%.4f px" y
  , printf "\\pdfhorigin=%.4f true cm" x0
  , printf "\\pdfvorigin=%.4f true cm" y0
  ]

rectangleBoundingBox :: (Double,Double) -> String
rectangleBoundingBox (x,y) = unlines
  [ printf "\\pgfpathrectangle{\\pgfpointorigin}{\\pgfqpoint{%.4fpx}{%.4fpx}}" x y
  , "\\pgfusepath{use as bounding box}"
  ]

latexPGFHeader :: (Double,Double) -> String
latexPGFHeader bounds =
  "\\begin{pgfpicture}\n" ++ rectangleBoundingBox bounds


latexPGFFooter :: String
latexPGFFooter = "\\end{pgfpicture}\n"

-- | A simple surface consisting of just a pgf picture
--
-- > \begin{pgfpicture}
-- >   <diagram>          
-- > \end{pgfpicture}
-- 
latexSurface :: Surface
latexSurface = Surface LaTeX False (\bounds -> (latexPGFHeader bounds,latexPGFFooter))


latexDocHeader :: String
latexDocHeader = unlines
  [ "\\documentclass{article}"
  , "\\usepackage{pgfcore}"
  ]


-- | A surface for a complete, compilable document
--
-- > \documentclass{article}
-- > \usepackage{pgfcore}
-- > \pdfpagewidth=200.0000 px
-- > \pdfpageheight=300.0000 px
-- > \pdfhorigin=-0.7120 true cm
-- > \pdfvorigin=0.0500 true cm
-- > \begin{document}
-- > \begin{pgfpicture}
-- >   \pgfpathrectangle{\pgfpointorigin}{\pgfqpoint{200.0000px}{300.0000px}}
-- >   \pgfusepath{use as bounding box}
-- >   <diagram>
-- > \end{pgfpicture}
-- > \end{document}
-- 
latexStandaloneSurface :: Surface
latexStandaloneSurface = Surface LaTeX True c
  where
    c bounds = (h,f)
      where
        h = latexDocHeader
            ++ pdfPageBounds bounds (-2.712, 1.85) -- obtained from trial and error
            ++ "\\begin{document}\n"
            ++ latexPGFHeader bounds
        f = latexPGFFooter
            ++ "\\end{document}\n"

-- | Same as 'complteDocSurface', but the created document is in a
-- preview mode
--
-- > \documentclass{article}
-- > \usepackage{pgf}
-- > \usepackage[graphics, active, tightpage]{preview}
-- > \PreviewEnvironment{pgfpicture}
-- > \begin{document}
-- > \begin{pgfpicture}
-- >    <diagram>
-- > \end{pgfpicture}            
-- > \end{document}
--    
latexPreviewSurface :: Surface
latexPreviewSurface = Surface LaTeX True c
  where
    c bounds = (h,f)
      where
        h = latexDocHeader
              ++ "\\usepackage[graphics, active, tightpage]{preview}\n"
              ++ "\\PreviewEnvironment{pgfpicture}\n"
              ++ "\\begin{document}\n"
              ++ latexPGFHeader bounds
        f = latexPGFFooter
              ++ "\\end{document}\n"

-- * ConTeXt surfaces

contextDocHeader :: String
contextDocHeader = "\\usemodule[pgfcore]"

contextPGFHeader :: (Double,Double) -> String
contextPGFHeader bounds = "\\startpgfpicture\n" ++ rectangleBoundingBox bounds
            
contextPGFFooter :: String
contextPGFFooter = "\\stoppgfpicture"

-- | A simple surface consisting of just a pgf picture
--
-- > \startpgfpicture
-- >   <diagram>          
-- > \stoppgfpicture
-- 
contextSurface :: Surface
contextSurface = Surface ConTeXt False (\bs -> (contextPGFHeader bs, contextPGFFooter))

-- | A surface for a complete, compilable ConTeXt document. Does not crop.
--
-- > \usepackage[pgf]
-- > \starttext
-- > \startpgfpicture
-- >   <diagram>
-- > \stoppgfpicture
-- > \stoptext
-- 
contextStandaloneSurface :: Surface
contextStandaloneSurface = Surface ConTeXt True c
  where
    c bounds = (h,f)
      where
        h = unlines
          [ contextDocHeader
          , "\\setuppagenumbering[location=]"
          , "\\starttext"
          , contextPGFHeader bounds
          ]
        f = unlines
          [ contextPGFFooter
          , "\\stoptext"
          ]

-- * Plain TeX surfaces

plaintexDocHeader :: String
plaintexDocHeader = unlines
  [ "\\input eplain"
  , "\\beginpackages"
  , "  \\usepackage{color}"
  , "\\endpackages"
  , "\\input pgfcore"
  ]
  

plaintexPGFHeader :: (Double,Double) -> String
plaintexPGFHeader bounds = "\\pgfpicture\n" ++ rectangleBoundingBox bounds

plaintexPGFFooter :: String
plaintexPGFFooter = "\\endpgfpicture"


-- | A simple surface consisting of just a pgf picture
--
-- > \pgfpicture
-- >   <diagram>          
-- > \endpgfpicture

plaintexSurface :: Surface
plaintexSurface = Surface PlainTeX False (\bs -> (plaintexPGFHeader bs, plaintexPGFFooter))

-- | A surface for a complete, compilable plain TeX document
--
-- > \input eplain
-- > \beginpackages
-- >   \usepackage{color}
-- > \endpackages
-- > \input pgfcore
-- > 
-- > \pgfpicture
-- > \pgfpathrectangle{\pgfpointorigin}{\pgfqpoint{200.0000px}{300.0000px}}
-- > \pgfusepath{use as bounding box}
-- >     <diagram>
-- > \endpgfpicture
-- > \bye
--
plaintexStandaloneSurface :: Surface
plaintexStandaloneSurface = Surface PlainTeX True c
  where
    c bounds = (h,f)
      where
        h = unlines
          [ plaintexDocHeader
          , pdfPageBounds bounds (-0.712, 0.02)
          , plaintexPGFHeader bounds
          ]
        f = unlines
          [ "\\endpgfpicture"
          , "\\bye"
          ]

