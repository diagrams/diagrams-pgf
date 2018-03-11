{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
import Diagrams.Prelude
import Diagrams.Backend.PGF

import Diagrams.Attributes
import Diagrams.TwoD.Attributes
import Geometry.Envelope
import Geometry.BoundingBox
import Geometry.TwoD.Combinators
import Diagrams.Types
-- import Diagrams.TwoD.Vector         (perp)

-- Example using TeX primatives to make a text box with given width. Also
-- includes roundedRect background and labeling.

bgs :: Diagram V2 -> Diagram V2
bgs = partsOf leafs %~ zipWith (\c d -> bg c d) (cycle [dodgerblue, orange, green, purple])

main :: IO ()
main = mainWithPGF $ example

mytext :: OnlineTex (Diagram V2)
mytext = scale 2 . box 8 orange <$> hboxOnline (sizedVBox 18 txt)
  where
    txt = "The sum of the squares of the lengths of the legs equals the square "
       ++ "of the length of the hypotenuse:"
       ++ "$$ a^2 + b^2 = c^2 .$$"

rightTriangle :: Diagram V2
rightTriangle
  = fromVertices [origin, mkP2 4 0, mkP2 4 3]
      # closeTrail
      # stroke
      # centerXY
      # scale 12
      # fc dodgerblue

labeledTriangle :: OnlineTex (Diagram V2)
labeledTriangle = scale 5 <$> do
  a <- hboxOnline "$a$"
  b <- hboxOnline "$b$"
  c <- hboxOnline "$c$"

  pure $ rightTriangle
           # label a unit_X
           # label b unit_Y
           # label c ((V2 4 3))

example :: OnlineTex (Diagram V2)
example = frame 10 <$> liftA2 (|-|) labeledTriangle mytext

--

(|-|) :: Diagram V2 -> Diagram V2 -> Diagram V2
a |-| b = a ||| strutX 25 ||| b

box :: Double -> Colour Double -> Diagram V2 -> Diagram V2
box padding colour content
  = centerXY content
 <> roundedRect w h 2
      # fc colour
  where
    V2 w h = (+padding) <$> size content

sizedVBox :: Show a => a -> String -> String
sizedVBox w x = "\\hsize=" ++ show w ++ "em\\vbox{\\noindent " ++ x ++ "}"

label :: Diagram V2 -> V2 Double -> Diagram V2 -> Diagram V2
label l v a = besideWithGap 3 (perp v) a (centerXY l)

-- is there a better way to do this?
besideWithGap :: Double -> V2 Double -> Diagram V2 -> Diagram V2 -> Diagram V2
besideWithGap g v a b = beside v a b'
  where
    b' = beside v (strut (g *^ signorm v)) b

