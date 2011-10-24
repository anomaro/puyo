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
import qualified Data.Field         as Field (sizeYyokokuLv3)

--------------------------------------------------------------------------------
--  �^
--------------------------------------------------------------------------------
data Kind   = Syo       -- ��
            | Chu       -- ��
            | Inseki    -- 覐�
            | Hoshi     -- ��
            | Tsuki     -- ��
            | Oukan     -- ����
            deriving (Show, Eq, Ord, Enum, Bounded)

type Shelf      = ( Fall Number.Puyo
                  , Fixed  Number.Puyo
                  , Progress Number.Puyo
                  )
{-  
    �\���Ղ�𔭐�������Ղ�����i�A���j���Ɉ�UProgress�ɒu����A
    ���̂Ղ����������������AProgress����Fixed�Ɉڂ��A
    ������܂Ղ悪�~�鑤�̃v���C���[�����n����������AFall�ɒu�����B
    Fall�ɒu���ꂽ�ʂ̂�����܂Ղ悪�ŏI�I�ɍ~���Ă���B
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
--  ���O
--------------------------------------------------------------------------------
initial  =  (Fall 0, Fixed 0, Progress 0)    :: Shelf

-- 覐΂Ղ悪�����炷������܂Ղ�̗ʁB
insekiVolume gs = ojamaVolume Inseki gs

--------------------------------------------------------------------------------
--  �֐�
--------------------------------------------------------------------------------
-- ���ۂɍ~�鐔
actual  :: Shelf -> Number.Puyo
actual (Fall n, _, _)    = n

-- ���v
total :: Shelf -> Number.Puyo
total (Fall n1, Fixed n2, Progress n3) = n1 + n2 + n3

-- �J��
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
--  ���
--------------------------------------------------------------------------------
-- �\��������
view        :: Number.Puyo -> Setting.Setting -> [Kind]
view n s    =  view' n (Setting.get Setting.FieldSizeX s) s
 where
    view' 0 _ _   = []
    view' _ 0 _   = []
    view' n x s   = kind : view' (n - ojamaVolume kind s) (x - 1) s
      where
        kind    = fromJust $ find ((0 <=) . (n -) . (flip ojamaVolume) s) ks
        ks      = [maxBound, pred maxBound .. minBound] :: [Kind]

--------------------------------------------------------------------------------
--  ��
--------------------------------------------------------------------------------
ojamaVolume :: Kind -> Setting.Setting -> Number.Puyo
ojamaVolume Syo    _                     = 1
ojamaVolume Chu    gs | rankSize gs > 1  = rankSize gs
ojamaVolume Inseki gs | rankSize gs > 1  = ojamaVolume Chu gs * Field.sizeYyokokuLv3
ojamaVolume Hoshi  gs | rankSize gs > 1  = ojamaVolume Inseki gs * rankSize gs
ojamaVolume k      gs | rankSize gs > 1  = ojamaVolume (pred k) gs * 2
ojamaVolume k      _                     = fromEnum k + 1

rankSize gs    = Setting.get Setting.FieldSizeX gs