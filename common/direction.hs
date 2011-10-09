module Common.Direction
( Area(..)
, Rotation(..)

, areas
, inversion
, compose
, rotate
) where

import Prelude   hiding (Left, Right)
import Data.Maybe       (fromJust)
import Rotatable

--------------------------------------------------------------------------------
--  Œ^
--------------------------------------------------------------------------------
-- •ûŒü
data Area       = Up | Right | Down | Left
        deriving (Show, Eq, Enum, Bounded)

data Rotation   = Clockwise | CounterClockwise
        deriving (Show, Eq, Enum, Bounded)

--------------------------------------------------------------------------------
--  –¼‘O
--------------------------------------------------------------------------------
areas   = [Up .. Left]  :: [Area]

--------------------------------------------------------------------------------
--  ƒNƒ‰ƒXŠÖ”
--------------------------------------------------------------------------------
class Directional d where
    inversion   :: d -> d

instance Directional Area where
    inversion   = fromJust . opposite

instance Directional Rotation where
    inversion   = fromJust . opposite

instance Rotatable Area where
    skip    = rotationDefaultFast

instance Rotatable Rotation where
    skip    = rotationDefaultFast

--------------------------------------------------------------------------------
--  ŠÖ”
--------------------------------------------------------------------------------
compose :: (Directional a, Eq a) => Maybe a -> Maybe a -> [a]
compose Nothing   (Just d2)                         =  [d2]
compose (Just d1) Nothing                           =  [d1]
compose (Just d1) (Just d2) | inversion d1 /= d2    =  [d1,d2]
compose _         _                                 =  []

rotate          :: Rotation -> Area -> Area
rotate Clockwise        d   = walk d
rotate CounterClockwise d   = backwalk d