module Lib.UI where

import Vis
import Linear (V3(V3), V4 (V4), V2 (V2))
import qualified Data.Text as T


uiXPlane = 1


type UIRect = V4 Float

uiSpace :: UIRect
uiSpace  = V4 (-1.2) (-1.2) 1.2 1.2


uiVec = V3 uiXPlane

fgVec = V3 (uiXPlane + 0.04)

uiBgCol = makeColor 0.15 0.1 0.1 0.95

uiFgCol = makeColor 0.85 0.8 0.8 0.85

uiNorm :: UIRect -> UIRect 
uiNorm (V4 y1 z1 y2 z2) = V4 ny1 nz1 ny2 nz2
        where
        (V4 yneg zneg ypos zpos) = uiSpace
        facy = (-yneg) + ypos
        facz = (-zneg) + zpos
        ny1 = y1 * facy + yneg
        ny2 = y2 * facy + yneg
        nz1 = z1 * facz + zneg
        nz2 = z2 * facz + zneg

uiPad = -0.02

uiLineHeight = 0.045

uiFrame = Line (Just 2) [uiVec yneg zneg, uiVec yneg zpos, uiVec ypos zpos, uiVec ypos zneg, uiVec yneg zneg] uiBgCol
        where
        (V4 yneg zneg ypos zpos) = uiSpace
 
uiBox :: UIRect ->  VisObject Float
uiBox rect = Quad (uiVec y1 z1) 
                  (uiVec y1 z2)
                  (uiVec y2 z2)
                  (uiVec y2 z1)
                  uiBgCol 
             where
             (V4 y1 z1 y2 z2) = uiNorm rect

uiTextBox :: UIRect -> T.Text -> VisObject Float
uiTextBox rect text = VisObjects [uiBox rect, Text3d (T.unpack text) (fgVec (y2+uiPad) (z2+uiPad)) Helvetica12 uiFgCol] 
            where
            (V4 y1 z1 y2 z2) = uiNorm rect
