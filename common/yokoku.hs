module Common.Yokoku
where

import qualified Common.Number          as Number
import qualified State.Setting          as Setting
import qualified Common.Field           as Field (sizeYyokokuLv3)

--------------------------------------------------------------------------------
--  型
--------------------------------------------------------------------------------
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

instance Functor Fall  where
    fmap f (Fall n)  = Fall (f n)
instance Functor Fixed   where
    fmap f (Fixed  n)  = Fixed  (f n)
instance Functor Progress  where
    fmap f (Progress n)  = Progress (f n)

--------------------------------------------------------------------------------
--  名前
--------------------------------------------------------------------------------
initial  =  (Fall 0, Fixed 0, Progress 0)    :: Shelf

-- 隕石ぷよがもたらすおじゃまぷよの量。
insekiVolume gs = ojamaVolume 3 gs

--------------------------------------------------------------------------------
--  関数
--------------------------------------------------------------------------------
-- 実際に降る数
actual  :: Shelf -> Number.Puyo
actual (Fall n, _, _)    = n

-- 合計
total :: Shelf -> Number.Puyo
total (Fall n1, Fixed n2, Progress n3) = n1 + n2 + n3

-- 遷移
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

--------------------------------------------------------------------------------
--  量
--------------------------------------------------------------------------------
ojamaVolume                         :: Int -> Setting.Setting -> Number.Puyo
ojamaVolume 1 _                     =  1
ojamaVolume 2 gs | rankSize gs > 1  =  ojamaVolume 1 gs * rankSize gs
ojamaVolume 3 gs | rankSize gs > 1  =  ojamaVolume 2 gs * Field.sizeYyokokuLv3
ojamaVolume 4 gs | rankSize gs > 1  =  ojamaVolume 3 gs * rankSize gs
ojamaVolume n gs | rankSize gs > 1  =  ojamaVolume (n - 1) gs * 2
ojamaVolume n _                     =  n

rankSize gs    = Setting.get Setting.FieldSizeX gs