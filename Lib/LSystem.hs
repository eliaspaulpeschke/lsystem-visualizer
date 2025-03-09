module Lib.LSystem where

import Vis (VisObject(VisObjects, Line), white)
import Linear (V3(V3), R1 (_x), R2 (_y), R3 (_z))
import Data.Array (range)
import Data.Char (Char)
import Graphics.Rendering.OpenGL (ColorTable(Texture1DColorTable))
import qualified Data.Text as T
import System.Posix.Internals (puts)
import System.Posix (PathVar(VDisableChar))
import Control.Lens

type Rules = Char -> T.Text 

produce :: T.Text -> Rules -> T.Text
produce input rules = T.foldl  (\res ch -> T.append res (rules ch)) (T.pack "") input

data Turtle = Turtle {
      tuPosition :: V3 Float
    , tuDirection :: V3 Float
} 

data DrawState = DrawState {
      stStack :: [Turtle]
    , stPictures :: [VisObject Float]
} 

type DrawRules = Char -> DrawState -> DrawState 

data Axis = X | Y | Z 

rotateV3 :: Axis -> Float -> V3 Float -> V3 Float
rotateV3 X a v = V3 x y z 
           where
           x = v ^._x
           y = (cos a * (v ^._y)) - (sin a * (v ^._z))
           z = (sin a * (v ^._y)) + (cos a * (v ^._z))
rotateV3 Y a v = V3 x y z 
           where
           x = (sin a * (v ^._x)) + (cos a * (v ^._z))
           y = v ^._y
           z = (sin a * (-(v ^._x))) + (cos a * (v ^._z))
rotateV3 Z a v = V3 x y z 
           where
           x = (cos a * (v ^._x)) - (sin a * (v ^._y))
           y = (sin a * (v ^._x)) + (cos a * (v ^._y))
           z = v ^._z

rotateSt :: Axis -> Float -> DrawState -> DrawState
rotateSt axis angle st = st { stStack = ( turtle { tuDirection = dir } ) : rest }
          where
          turtle = head $ stStack st
          rest = tail $ stStack st
          oldDir =  tuDirection turtle
          rad = (pi * angle) / 180 
          dir = rotateV3 axis angle oldDir

moveSt :: DrawState -> DrawState
moveSt st = st { stStack = ( turtle { tuPosition = newPos } ) : rest }
          where
          turtle = head $ stStack st
          rest = tail $ stStack st
          pos = tuPosition turtle
          dir = tuDirection turtle
          newPos = pos + dir 

lineSt :: DrawState -> DrawState
lineSt st = st { stStack = ( turtle { tuPosition = newPos } ) : rest, 
                 stPictures =  Line Nothing [pos, newPos] white : stPictures st }
            where
            turtle = head $ stStack st
            rest = tail $ stStack st
            pos = tuPosition turtle
            dir = tuDirection turtle
            newPos = pos + dir

pushSt :: DrawState -> DrawState
pushSt st = st { stStack = new : stStack st }
            where
            last = head $ stStack st
            new = Turtle { tuPosition = tuPosition last, tuDirection = tuDirection last }

popSt :: DrawState -> DrawState
popSt st = if length (stStack st) < 2 
             then st 
             else st { stStack = tail $ stStack st }  
draw :: T.Text -> DrawState -> DrawRules -> VisObject Float 
draw input init rules = VisObjects $ stPictures (T.foldl (flip rules) init input)
