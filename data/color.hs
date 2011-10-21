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
--  �^
--------------------------------------------------------------------------------
data Color  = Red 
            | Green
            | Blue 
            | Yellow 
            | Purple 
        deriving (Show, Eq, Ord, Enum, Bounded)

type ColorAssortment    = Vector.Vector Color

--------------------------------------------------------------------------------
--  ���O
--------------------------------------------------------------------------------
colors      = [minBound .. maxBound]            :: [Color]
maxNumber   = fromEnum (maxBound :: Color) + 1  :: Number.Colors

--------------------------------------------------------------------------------
--  �֐�
--------------------------------------------------------------------------------
inGLUT          :: Color -> GLUT.Color3 Double
inGLUT Red      =  GLUT.Color3 1.0 0.0 0.0
inGLUT Green    =  GLUT.Color3 0.0 1.0 0.2
inGLUT Blue     =  GLUT.Color3 0.0 0.3 1.0
inGLUT Yellow   =  GLUT.Color3 1.0 1.0 0.0
inGLUT Purple   =  GLUT.Color3 0.7 0.0 1.0

-- �F�����߂�B
determine       :: ColorAssortment -> Int -> Color
determine cs n  =  cs Vector.! n

-- �F�̏���̗�
assortments :: [ ColorAssortment ]
assortments =  map Vector.fromList $ permutation colors

totalAssortments n    = product [1 .. n]

-- ����
permutation     :: (Eq a) => [a] -> [[a]]
permutation []  = [[]]
permutation xs  = [y:ys | y <- xs, ys <- permutation (delete y xs)]