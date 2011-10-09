-- file: typedata.hs
module Common.DataType
     where

import Common.Time  (Time)

--------------------------------------------------------------------------------
--  場面遷移
--------------------------------------------------------------------------------
data Period = Game GameName -- ゲーム（ぷよぷよ）
            | Configuration -- ゲームの設定

data GameName   = PuyoPuyo  -- 通常のぷよぷよ

--------------------------------------------------------------------------------
--  ゲーム
-------------------------------------------------------------------------------- 
-- フィールド座標。( Ｙ座標, Ｘ座標 )
type AreaPosition   = (PositionY, PositionX)
type PositionY  = Int
type PositionX  = Int 

-- 数
type NumOfPuyos     = Int   -- 組みぷよの数
type NumOfPuyo      = Int   -- ぷよの数
type NumOfUnion     = Int   -- ぷよの結合数 (ぷよの数と同じ？)
type NumOfColors    = Int   -- 色の数
type NumOfChain     = Int   -- 連鎖数

--------------------------------------------------------------------------------
--  ゲームの状態遷移
--------------------------------------------------------------------------------
-- ゲームの状態遷移を表すデータ。
data GamePhase  = BuildPhase    -- ぷよ生成
                | ControlPhase  -- プレイヤーの操作
                | DropPhase     -- ちぎりやぷよ消滅後に起こるぷよ落下
                | ErasePhase    -- ぷよ消滅
                | ErasePhase'
                | FallPhase     -- おじゃまぷよの落下
                | FallPhase'
                | GameOverPhase -- ゲームオーバー
                | AnimationPhase  Time GamePhase  -- 硬直時間と次の状態遷移
        deriving (Show, Eq)