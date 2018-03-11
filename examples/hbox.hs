{-# LANGUAGE FlexibleContexts #-}
import Diagrams.Backend.PGF
import Diagrams.Prelude
import Geometry.TwoD.Vector         (perp)

-- import Diagrams.Size

type D2 = Diagram V2

-- The simplest way to construct a hbox with an envelope is to use
--
-- envelopedText = surfHbox mysurf "text" :: Diagram PGF V2 Double
--
-- which can be used just like any other diagram. However, each box like this
-- makes a call to TeX, so for multiple boxes this can become slow.
--
-- This examples shows how to use the OnlineTeX monad to construct a diagram
-- with multiple hboxs with a single call to TeX.

main = main1

-- The simplest option. The command line gives the option to use the default
-- latex, context or plain tex surface (latex is the default).
main1 = onlineMain example

-- Similar to 'onlineMain' but uses the specified surface.
main2 = onlineMainWithSurf (with & command .~ "lualatex") example

-- Alternatively, 'renderOnlinePDF' can be used to directly generate a pdf.
main3 = renderOnlinePGF "hbox.pdf" absolute example

-- "renderOnlinePDF'" allows choosing rendering options
main4 = renderOnlinePGF' "hbox.tex"
                         (with & sizeSpec .~ mkWidth 300 & standalone .~ True)
                         example

example :: OnlineTex D2
example = frame 5 . scale 10 <$> hboxLines "\\TeX"

-- Use the envelope from the hbox to label the width, height and depth.
hboxLines :: String -> OnlineTex D2
hboxLines str = do
  txt <- hboxOnline str

  let h = envelopeV unitY txt
      d = envelopeV unit_Y txt
      w = envelopeV unitX txt

  hArrow <- labeledArrow False "height" h
  dArrow <- labeledArrow True "depth" d
  wArrow <- labeledArrow False "width" w

  return $ (txt <> boundingRect txt <> fromOffsets [w])
           ||| strutX 1 ||| (hArrow === dArrow)
           === strutY 1
           === wArrow

--

-- Draw an arrow with a horizontal label above or below.
labeledArrow :: Bool -> String -> V2 Double -> OnlineTex D2
labeledArrow above label r = diaArrow <$> hboxOnline label
  where
    diaArrow txt = atDirection ((direction . rev . perp) r)
                               (arr # translate (negated $ r^/2))
                               (txt # centerXY # scale 0.1 # frame 0.3)
                               # translate (r^/2)
      where
        rev = if above then id else negated
        arr = arrowV' ops r
        ops = with & arrowHead .~ spike
                   & arrowTail .~ spike'
                   & lengths   .~ normalized 0.015


