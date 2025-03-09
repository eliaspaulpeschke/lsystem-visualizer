import Lib.LSystem
import Vis
import Linear (V3(V3), R1 (_x), R2 (_y), R3 (_z))
import Data.Array (range)
import Data.Char (Char)
import Graphics.Rendering.OpenGL (ColorTable(Texture1DColorTable))
import qualified Data.Text as T
import System.Posix.Internals (puts)
import System.Posix (PathVar(VDisableChar))
import Control.Lens

prod x = produce x plantRules 

dr x = draw x plantDrawRules

drawScale = 1.5 

times :: Int -> (a -> a) -> (a -> a)
times 1 f = f
times x f = f . times (x - 1) f

code = times 6 prod $ T.pack "---X"

myPlant = VisObjects [draw code initSt plantDrawRules, draw code initSt dr2]

opts = defaultOpts { optAntialiasing = Multisampled 4}

main = animate opts (\x -> RotEulerDeg (Euler {ePitch = 0, eYaw = 30*x, eRoll = 0} )myPlant)  

initSt = DrawState {
      stStack = [ Turtle { 
                    tuPosition = V3 0 0 1 
                  , tuDirection = V3 0 0 (-0.015)} ]
    , stPictures = [] --Axes (1, 10)]
}


plantRules :: Rules
plantRules 'X' = T.pack "F+[[X]-X]-F[-FX]+X"
plantRules 'F' = T.pack "FF"
plantRules a = T.pack [a]

plantDrawRules :: DrawRules
plantDrawRules 'F' = lineSt   
plantDrawRules '-' = rotateSt X (-25)  
plantDrawRules '+' = rotateSt X 25.2  -- . rotateSt Z (-0.1)
plantDrawRules '[' = pushSt . rotateSt Z 0.5
plantDrawRules ']' = popSt
plantDrawRules _ = id

dr2 :: DrawRules
dr2 '-' = rotateSt X (-25)
dr2 '+' = rotateSt X 25.1
dr2 '[' = pushSt . rotateSt Y 0.5
dr2 x = plantDrawRules x
