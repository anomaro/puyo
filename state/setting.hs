module State.Setting
( GameState
, get
, get_ColorPattern
, newGameState

, GameStateIndex (..)
, GameStateValue
, initialGameState

, yokokuLv1
, yokokuLv2
, yokokuLv3
, yokokuLv4
, yokokuLv5
, yokokuLv6

, flag_quickTrun
, flag_oturi
) where

-- ゲーム環境の設定等を決める値
-- 色数・ぷよの消える数・おじゃまぷよレートなどなど。。。

import qualified Common.Number          as Number
import qualified Common.Color           as Color
import qualified Common.Random          as Random (run)

import qualified Data.Vector.Unboxed    as VCU

--------------------------------------------------------------------------------
--  型
--------------------------------------------------------------------------------
data GameState = GameState GameStateValues      -- Int
                           ColorPattern         -- 色パターン

type GameStateValues    = VCU.Vector GameStateValue
type GameStateValue     = Int
data GameStateIndex     -- GameStateの値の種類を表す。
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
defaultGameState :: GameStateValues
defaultGameState =  VCU.generate numOfGSI $ defaultValue . toEnum

initialGameState    :: GameState
initialGameState    =  GameState defaultGameState 
                                 (randomColorPattern $ limitValue max Color)

-- 各値の初期値
defaultValue                :: GameStateIndex -> GameStateValue
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

headGSI = minBound  :: GameStateIndex
lastGSI = maxBound  :: GameStateIndex

--------------------------------------------------------------------------------
--  読み取り
--------------------------------------------------------------------------------
-- 値を取り出す。 ・・・値の種類（GameStateIndex）を指定して、GameStateから取り出す。
get :: GameStateIndex -> GameState          -> GameStateValue
get =  \i             -> \(GameState v _)   -> v VCU.! fromEnum i

-- 色パターン
get_ColorPattern    :: GameState        -> ColorPattern
get_ColorPattern =  \(GameState _ c)    -> c

--------------------------------------------------------------------------------
--  書き換え
--------------------------------------------------------------------------------
-- 値を書き換える
newGameState    :: (GameStateValue -> GameStateValue) -> GameStateIndex 
                -> GameState -> GameState
newGameState f i gs@(GameState gv cp)   = GameState gv' cp
  where
    gv' = gv VCU.// [(i', e')]
    i'  = fromEnum i
    e'  | e'' > limitValue max i    = limitValue min i
        | e'' < limitValue min i    = limitValue max i
        | otherwise                 = e''
    e'' = f $ get i gs

--------------------------------------------------------------------------------
--  予告ぷよ
--------------------------------------------------------------------------------
yokokuLv1    = 1
yokokuLv2 gs    | fieldSizeX > 1    = yokokuLv1 * fieldSizeX
                | otherwise         = 2
  where fieldSizeX = get FieldSizeX gs
--yokokuLv3 gs    | fieldSizeX > 1    = Field.sizeYyokokuLv3 * yokokuLv2 gs
yokokuLv3 gs    | fieldSizeX > 1    = 5 * yokokuLv2 gs
                | otherwise         = 3
  where fieldSizeX = get FieldSizeX gs
yokokuLv4 gs    | fieldSizeX > 1    = fieldSizeX * yokokuLv3 gs
                | otherwise         = 4
  where fieldSizeX = get FieldSizeX gs
yokokuLv5 gs    | fieldSizeX > 1    = 2 * yokokuLv4 gs
                | otherwise         = 5
  where fieldSizeX = get FieldSizeX gs
yokokuLv6 gs    | fieldSizeX > 1    = 2 * yokokuLv5 gs
                | otherwise         = 6
  where fieldSizeX = get FieldSizeX gs

--------------------------------------------------------------------------------
--  色パターン
--------------------------------------------------------------------------------
-- ランダムな色パターンを作る。
randomColorPattern  :: Number.Colors -> ColorPattern
randomColorPattern  =  \noc          -> do
    val     <- Random.run $ Color.totalAssortments noc - 1
    return $ Color.assortments !! val

--------------------------------------------------------------------------------
--  ゲーム基本設定
--------------------------------------------------------------------------------
flag_quickTrun      = False :: Bool         -- クイックターンの有無
flag_oturi          = True  :: Bool         -- おじゃまぷよのおつりの有無