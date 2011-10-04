module Common.Area
where

import Common.DataType
import Common.Name

--------------------------------------------------------------------------------
--  型
--------------------------------------------------------------------------------
-- エリア（フィールドの１マス分をそう呼ぶことにする）を構成するオブジェクトの種類。
data Area   
    = Space
    | Puyo  { color :: Color, union :: UnionCheck, anime :: AnimationType }
    | Ojama {                 union :: UnionCheck, anime :: AnimationType }
    | Wall
        deriving (Show, Eq)

-- ぷよの結合チェック状態。
data UnionCheck = NotYet        -- 未調査
                | Completion    -- 調査完了
                | EraseFlag     -- 消滅フラグ
        deriving (Show, Eq)
        
-- フィールドぷよのアニメーションの状態を表すデータ。
data AnimationType  = Normal                -- アニメーション無し
                    | Dropping   Time       -- 落下時
                    | Landing    Time Power -- 着地時
                    | Erasing    Time       -- 消滅時
--                    | Projecting Time       -- 消滅予測時
        deriving (Show, Eq)

type Power  = Double        -- アニメーションの強さ。

--------------------------------------------------------------------------------
--  名前
--------------------------------------------------------------------------------
initialFst  = Wall :: Area
initialSnd  = Space :: Area

animeStartDropping  =  Dropping amimeTime_drop    :: AnimationType
animeStartErasing   =  Erasing  amimeTime_erase   :: AnimationType

animeStartLanding   :: Power  -> AnimationType
animeStartLanding p =  Landing amimeTime_land p

defauletPower   = 3 :: Power

defaultOjamaPuyo    =  defaultState Ojama   :: Area

--------------------------------------------------------------------------------
--  関数
--------------------------------------------------------------------------------
-- オブジェクト判定
isPuyo                  :: Area -> Bool
isPuyo (Puyo  _ _ _)    =  True
isPuyo (Ojama   _ _)    =  True
isPuyo _                =  False

is_colorPuyo                :: Area -> Bool
is_colorPuyo (Puyo _ _ _)   =  True
is_colorPuyo _              =  False

isSpace         :: Area -> Bool
isSpace Space   =  True
isSpace _       =  False

-- アニメーション時間をカウント
countAT                 :: Area -> Area
countAT (Puyo c uc at)  =  Puyo c uc $ count at
countAT (Ojama  uc at)  =  Ojama  uc $ count at
countAT a               =  a

count                           :: AnimationType -> AnimationType
count ( Dropping n   )  | n > 0 =  Dropping $ n - 1
count ( Landing  n p )  | n > 0 =  Landing  ( n - 1 ) p
count ( Erasing  n   )  | n > 0 =  Erasing  $ n - 1
count _                         =  Normal

-- デフォルト適用
defaultState    :: (UnionCheck -> AnimationType -> Area) -> Area
defaultState    =  ($ Normal) . ($ NotYet)
