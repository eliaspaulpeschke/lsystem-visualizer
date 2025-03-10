import Lib.LSystem
import Vis
import Linear (V3(V3), V4(V4))
import qualified Data.Text as T
import Lib.UI (uiBox, uiFrame, uiTextBox)

prod x = produce x plantRules 

times :: Int -> (a -> a) -> (a -> a)
times 1 f = f
times x f = f . times (x - 1) f

code = times 6 prod $ T.pack "---X"

myPlant = VisObjects [draw code initSt plantDrawRules, draw code initSt dr2]

opts = defaultOpts { optAntialiasing = Multisampled 4, optInitialCamera = Just $ Camera0 0 0 5 }

main = display opts $ VisObjects [ myPlant, uiTextBox (V4 0 0.05 0.1 0.1) (T.pack "Hello"), uiFrame]



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
