{-# LANGUAGE NoMonomorphismRestriction #-}
module GroupTransparency where

import Diagrams.Prelude
import Diagrams.Backend.PGF.CmdLine

-- Example of group opacity.

type D2 = Diagram PGF V2 Double

main = defaultMain (frame 10 xs)

xs :: D2
xs = x # opacity 0.3 ||| strutX 10 ||| x # opacityGroup 0.3
  where
    x = lw 10 $ (origin ~~ pure 40) <> (mkP2 40 0 ~~ mkP2 0 40)

