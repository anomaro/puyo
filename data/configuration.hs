-- configuration.hs
module Data.Configuration
where

import Data.Maybe (isJust, fromJust)

import qualified Data.Setting      as Setting (Item (..), Setting, get)
import qualified Data.Time        as Time (Time, inputConfig)

--------------------------------------------------------------------------------
--  型
--------------------------------------------------------------------------------
type Selection  = Entry     -- 選択中の項目

data Entry  = FallTime      -- 落下時間
            | ErasePuyo     -- ぷよの消える数
            | Color         -- ぷよの色の数
            | OjamaRate     -- おじゃまぷよレート
            | MarginTime    -- マージンタイム
            | FieldSizeY    -- フィールドサイズ（縦）（可視範囲のサイズ）
            | FieldSizeX    -- フィールドサイズ（横）（可視範囲のサイズ）
            | NextPuyoView  -- ネクストぷよ以降の表示数
        deriving (Eq, Enum, Bounded)

data ConfiguPhase   = SelectPhase
                    | AnimationPhase Time.Time

defaultConfigAnimePhase = AnimationPhase Time.inputConfig

--------------------------------------------------------------------------------
--  設定データ参照
--------------------------------------------------------------------------------
-- 項目の列挙
listEntry   = enumFrom $ initialEntry   :: [Entry]

initialEntry    = minBound              :: Entry    -- 一番最初の項目
lastEntry       = maxBound              :: Entry    -- 一番最後の項目

-- 項目名
toEntryName :: Entry -> String
toEntryName FallTime        =  "Fall Time"
toEntryName ErasePuyo       =  "Erases"
toEntryName Color           =  "Colors"
toEntryName OjamaRate       =  "OjamaPuyoRate"
toEntryName MarginTime      =  "MarginTime"
toEntryName FieldSizeY      =  "FieldSize(Height)"
toEntryName FieldSizeX      =  "FieldSize(Width)"
toEntryName NextPuyoView    =  "NextPuyo"

-- ゲーム環境の値との対応
toGameStateIndex    :: Entry -> Maybe Setting.Item
toGameStateIndex FallTime       =  Just Setting.FallTime
toGameStateIndex ErasePuyo      =  Just Setting.ErasePuyo
toGameStateIndex Color          =  Just Setting.Color
toGameStateIndex OjamaRate      =  Just Setting.OjamaRate
toGameStateIndex MarginTime     =  Just Setting.MarginTime
toGameStateIndex FieldSizeY     =  Just Setting.FieldSizeY
toGameStateIndex FieldSizeX     =  Just Setting.FieldSizeX
toGameStateIndex NextPuyoView   =  Just Setting.NextPuyoView
--toGameStateIndex _              =  Nothing

-- ゲーム環境の値を得る。
getGameStateValue           :: Entry -> Setting.Setting -> String
getGameStateValue ent
  | isJust index    = show . Setting.get (fromJust index) 
  | otherwise       = const "" 
  where
    index   = toGameStateIndex ent
    
-- Selection操作
succSelection   :: Selection -> Selection
succSelection sl
    | sl == lastEntry       = minBound
    | otherwise             = succ sl

predSelection   :: Selection -> Selection
predSelection sl
    | sl == initialEntry    = maxBound
    | otherwise             = pred sl