module Data.Setting
( Setting
, Item (..)
, Value
, get
, getColorPattern
, renew
, initial
, flag_quickTrun
, flag_oturi
) where

import qualified Data.Vector.Unboxed    as VCU

import qualified Data.Number          as Number
import qualified Data.Color           as Color
import qualified Data.Random          as Random (run)

--------------------------------------------------------------------------------
--  型
--------------------------------------------------------------------------------
data Setting   = Setting Values ColorPattern

type Values    = VCU.Vector Value
type Value     = Int
data Item     -- GameStateの値の種類を表す。
        = FallTime      -- 落下時間
        | ErasePuyo     -- ぷよの消える数
        | Color         -- ぷよの色の数
        | OjamaRate     -- おじゃまぷよレート
        | MarginTime    -- マージンタイム
        | FieldSizeY    -- フィールドサイズ（縦）（可視範囲のサイズ）
        | FieldSizeX    -- フィールドサイズ（横）（可視範囲のサイズ）
        | NextPuyoView  -- ネクストぷよ以降の表示数
        deriving (Show, Eq, Ord, Enum, Bounded)
        
type ColorPattern   = IO Color.ColorAssortment

--------------------------------------------------------------------------------
--  既定値
--------------------------------------------------------------------------------
-- 各値の初期値
defaultValue                :: Item -> Value
defaultValue FallTime       =  40
defaultValue ErasePuyo      =  4
defaultValue Color          =  4
defaultValue OjamaRate      =  70
defaultValue MarginTime     =  96  -- ※秒単位の数値
defaultValue FieldSizeY     =  12
defaultValue FieldSizeX     =  6
defaultValue NextPuyoView   =  1

-- 境界数
limitValue f FallTime       = f 1   100
limitValue f ErasePuyo      = f 1   100
limitValue f Color          = f 1   Color.maxNumber
limitValue f OjamaRate      = f 1   100
limitValue f MarginTime     = f 1   300
limitValue f FieldSizeY     = f 1   48
limitValue f FieldSizeX     = f 1   24
limitValue f NextPuyoView   = f 0   6

-- GameStateの値の種類（GameStateIndex）の値の総数。
numOfGSI    = 1 + fromEnum lastGSI  :: Int

headGSI = minBound  :: Item
lastGSI = maxBound  :: Item

--------------------------------------------------------------------------------
--  初期値
--------------------------------------------------------------------------------
initial :: Setting
initial =  Setting values (randomColorPattern $ limitValue max Color)
  where
    values   =  VCU.generate numOfGSI $ defaultValue . toEnum :: Values

    randomColorPattern      :: Number.Colors -> ColorPattern
    randomColorPattern  noc =  do
        val     <- Random.run $ Color.totalAssortments noc - 1
        return $ Color.assortments !! val

--------------------------------------------------------------------------------
--  読み取り
--------------------------------------------------------------------------------
-- 値を取り出す。 ・・・値の種類（GameStateIndex）を指定して、GameStateから取り出す。
get ::  Item -> Setting          -> Value
get =  \i             -> \(Setting v _)   -> v VCU.! fromEnum i

-- 色パターン
getColorPattern    :: Setting -> ColorPattern
getColorPattern =  \(Setting _ c)    -> c

--------------------------------------------------------------------------------
--  書き換え
--------------------------------------------------------------------------------
-- 値を書き換える
renew    :: (Value -> Value) -> Item -> Setting -> Setting
renew f i gs@(Setting gv cp)   = Setting gv' cp
  where
    gv' = gv VCU.// [(i', e')]
    i'  = fromEnum i
    e'  | e'' > limitValue max i    = limitValue min i
        | e'' < limitValue min i    = limitValue max i
        | otherwise                 = e''
    e'' = f $ get i gs

--------------------------------------------------------------------------------
--  ゲーム基本設定
--------------------------------------------------------------------------------
flag_quickTrun      = False :: Bool         -- クイックターンの有無
flag_oturi          = True  :: Bool         -- おじゃまぷよのおつりの有無