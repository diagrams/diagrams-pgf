import Control.Lens ((<&>))

import Diagrams.Prelude
import Diagrams.Backend.PGF.CmdLine

type D2 = Diagram PGF

maxSum = 6 :: Int

sumTo n = show $ (n * (n + 1)) `div` 2

-- The 'OnlineTex' monad will use the given surface to run TeX commands
mkSum :: Int -> OnlineTex D2
mkSum n = hboxOnline (displayStyle tex)
           <&> centerXY
           <&> named n
 where
   tex | n == maxSum = sumTo maxSum
       | otherwise   = sumTo n ++ " + \\sum_{i=" ++ show (n+1) ++ "}^{" ++ show maxSum ++ "} i"

onlineDiagram :: OnlineTex D2
onlineDiagram = do
  sums <- mapM mkSum [0..maxSum]
  let maxHeight = maximum $ map height sums
      sumsCat   = cat' (V2 1 (-2))
                       (with & sep .~ (maxHeight * 2)
                             & catMethod .~ Distrib
                       ) sums
      arrowed   = foldr (\i -> connectOutside' arrowOpts i (i+1))
                        sumsCat
                        [0..maxSum - 1]

  return $ frame 20 arrowed

-- 'onlineMain' takes a diagram wrapped in 'OnlineTeX'
main = onlineMain onlineDiagram

--

arrowOpts
  = with & shaftStyle %~ lw thin
         & gaps       .~ local 3
         & headLength .~ local 5

displayStyle tex = "$\\displaystyle " ++ tex ++ "$"

