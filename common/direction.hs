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
--  型
--------------------------------------------------------------------------------
-- 方向
data Area       = Up | Right | Down | Left
        deriving (Show, Eq, Enum, Bounded)

data Rotation   = Clockwise | CounterClockwise
        deriving (Show, Eq, Enum, Bounded)

--------------------------------------------------------------------------------
--  名前
--------------------------------------------------------------------------------
areas   = [Up .. Left]  :: [Area]

--------------------------------------------------------------------------------
--  クラス関数
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
--  関数
--------------------------------------------------------------------------------
compose :: (Directional a, Eq a) => Maybe a -> Maybe a -> [a]
compose Nothing   (Just d2)                         =  [d2]
compose (Just d1) Nothing                           =  [d1]
compose (Just d1) (Just d2) | inversion d1 /= d2    =  [d1,d2]
compose _         _                                 =  []

rotate          :: Rotation -> Area -> Area
rotate Clockwise        d   = walk d
rotate CounterClockwise d   = backwalk d