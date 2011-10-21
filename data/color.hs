module Data.Color
( Color
, ColorAssortment
, maxNumber
, inGLUT
, determine
, assortments
, totalAssortments
)where

import Data.List
import qualified Data.Vector            as Vector
import qualified Graphics.UI.GLUT       as GLUT (Color3 (Color3))

import qualified Data.Number          as Number
--------------------------------------------------------------------------------
--  型
--------------------------------------------------------------------------------
data Color  = Red 
            | Green
            | Blue 
            | Yellow 
            | Purple 
        deriving (Show, Eq, Ord, Enum, Bounded)

type ColorAssortment    = Vector.Vector Color

--------------------------------------------------------------------------------
--  名前
--------------------------------------------------------------------------------
colors      = [minBound .. maxBound]            :: [Color]
maxNumber   = fromEnum (maxBound :: Color) + 1  :: Number.Colors

--------------------------------------------------------------------------------
--  関数
--------------------------------------------------------------------------------
inGLUT          :: Color -> GLUT.Color3 Double
inGLUT Red      =  GLUT.Color3 1.0 0.0 0.0
inGLUT Green    =  GLUT.Color3 0.0 1.0 0.2
inGLUT Blue     =  GLUT.Color3 0.0 0.3 1.0
inGLUT Yellow   =  GLUT.Color3 1.0 1.0 0.0
inGLUT Purple   =  GLUT.Color3 0.7 0.0 1.0

-- 色を決める。
determine       :: ColorAssortment -> Int -> Color
determine cs n  =  cs Vector.! n

-- 色の順列の列挙
assortments :: [ ColorAssortment ]
assortments =  map Vector.fromList $ permutation colors

totalAssortments n    = product [1 .. n]

-- 順列
permutation     :: (Eq a) => [a] -> [[a]]
permutation []  = [[]]
permutation xs  = [y:ys | y <- xs, ys <- permutation (delete y xs)]