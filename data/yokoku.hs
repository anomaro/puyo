module Data.Yokoku
( Kind (..)
, Shelf
, initial
, insekiVolume
, actual
, total
, fixed
, fall
, add
, exhale
, reset
, view
) where

import Data.List (find)
import Data.Maybe (fromJust)

import qualified Data.Number        as Number
import qualified Data.Setting       as Setting
import qualified Data.Field         as Field (rankInseki)

--------------------------------------------------------------------------------
--  型
--------------------------------------------------------------------------------
data Kind   = Syo       -- 小
            | Chu       -- 中
            | Inseki    -- 隕石
            | Hoshi     -- 星
            | Tsuki     -- 月
            | Oukan     -- 王冠
            deriving (Show, Eq, Ord, Enum, Bounded)

type Shelf      = ( Fall Number.Puyo
                  , Fixed  Number.Puyo
                  , Progress Number.Puyo
                  )
{-  
    予告ぷよを発生させるぷよ消去（連鎖）中に一旦Progressに置かれ、
    そのぷよ消去が完了したら、ProgressからFixedに移し、
    おじゃまぷよが降る側のプレイヤーが着地完了したら、Fallに置かれる。
    Fallに置かれた量のおじゃまぷよが最終的に降ってくる。
-}
newtype Fall a = Fall a
newtype Fixed  a = Fixed  a
newtype Progress a = Progress a

--------------------------------------------------------------------------------
--  名前
--------------------------------------------------------------------------------
initial  =  (Fall 0, Fixed 0, Progress 0)    :: Shelf

-- 隕石ぷよがもたらすおじゃまぷよの量。
insekiVolume gs = ojamaVolume Inseki gs

--------------------------------------------------------------------------------
--  関数
--------------------------------------------------------------------------------
actual  :: Shelf -> Number.Puyo
actual (Fall n, _, _)    = n

total :: Shelf -> Number.Puyo
total (Fall n1, Fixed n2, Progress n3) = n1 + n2 + n3

fixed :: Shelf -> Shelf
fixed (n, Fixed n2, Progress n3)    = (n, Fixed $ n2 + n3, Progress 0)

fall :: Number.Puyo -> Shelf -> Shelf
fall m (Fall n1, Fixed n2, n)
    | n2 > m    = (Fall $ n1 + m , Fixed $ n2 - m, n)
    | otherwise = (Fall $ n1 + n2, Fixed 0       , n)

add                         :: Number.Puyo -> Shelf -> Shelf
add n (a, b, Progress c)    =  (a, b, Progress (c + n))

exhale  :: Number.Puyo -> Shelf -> Shelf
exhale n (Fall a, b, c) = (Fall (a - n), b, c)

reset :: Shelf -> Shelf
reset (a, Fixed b, c)    =  (a, Fixed (b + 1), c)

view        :: Number.Puyo -> Setting.Setting -> [Kind]
view n s    =  take (Setting.get Setting.FieldSizeX s) $ view' n s
 where
    view' 0 _   = []
    view' n s   = kind : view' (n - ojamaVolume kind s) s
      where
        kind    = fromJust $ find ((0 <=) . (n -) . (flip ojamaVolume) s) ks
        ks      = [maxBound, pred maxBound .. minBound] :: [Kind]

ojamaVolume :: Kind -> Setting.Setting -> Number.Puyo
ojamaVolume Syo    _                     = 1
ojamaVolume Chu    gs | rankSize gs > 1  = rankSize gs
ojamaVolume Inseki gs | rankSize gs > 1  = ojamaVolume Chu gs * Field.rankInseki
ojamaVolume Hoshi  gs | rankSize gs > 1  = ojamaVolume Inseki gs * rankSize gs
ojamaVolume k      gs | rankSize gs > 1  = ojamaVolume (pred k) gs * 2
ojamaVolume k      _                     = fromEnum k + 1

rankSize gs    = Setting.get Setting.FieldSizeX gs