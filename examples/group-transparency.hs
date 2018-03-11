{-# LANGUAGE FlexibleContexts #-}
module GroupTransparency where

import Diagrams.Prelude
import Diagrams.Backend.CmdLine
import Diagrams.Backend.PGF

-- Example of group opacity.

-- type D2 = Diagram PGF

main = mainWith PGF (frame 10 xs)

a ~~ b = fromVertices [a,b]

xs :: Diagram V2
xs = x # opacity 0.3 ||| strutX 10 ||| x # groupOpacity 0.3
  where
    x = lw 10 $ (origin ~~ pure 40) <> (mkP2 40 0 ~~ mkP2 0 40)

