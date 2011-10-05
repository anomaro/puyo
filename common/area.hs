module Common.Area
where

import Common.DataType
import Common.Name

import Standardizable

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
instance Standardizable AnimationType where
    standard        = Normal

type Power  = Double        -- アニメーションの強さ。

--------------------------------------------------------------------------------
--  名前
--------------------------------------------------------------------------------
initialFst  = Wall :: Area
initialSnd  = Space :: Area

defaultOjamaPuyo    =  defaultState Ojama   :: Area

animeStartDropping  =  Dropping amimeTime_drop    :: AnimationType
animeStartErasing   =  Erasing  amimeTime_erase   :: AnimationType

animeStartLanding   :: Power  -> AnimationType
animeStartLanding p =  Landing amimeTime_land p

defauletPower   = 3 :: Power

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

-- 部分書き換え
modifyColor                 :: (Color -> Color) -> Area -> Area
modifyColor f (Puyo c u a)  =  Puyo (f c) u a
modifyColor _ area          =  area

modifyUnion                 :: (UnionCheck -> UnionCheck) -> Area -> Area
modifyUnion f (Puyo c u a)  =  Puyo c (f u) a
modifyUnion f (Ojama  u a)  =  Ojama  (f u) a
modifyUnion _ area          =  area

modifyAnime                 :: (AnimationType -> AnimationType) -> Area -> Area
modifyAnime f (Puyo c u a)  =  Puyo c u (f a)
modifyAnime f (Ojama  u a)  =  Ojama  u (f a)
modifyAnime _ area          =  area

-- アニメーション時間をカウント
countAT :: Area -> Area
countAT = modifyAnime count

count                           :: AnimationType -> AnimationType
count ( Dropping n   )  | n > 0 =  Dropping $ n - 1
count ( Landing  n p )  | n > 0 =  Landing  ( n - 1 ) p
count ( Erasing  n   )  | n > 0 =  Erasing  $ n - 1
count _                         =  Normal

-- デフォルト適用
defaultState    :: (UnionCheck -> AnimationType -> Area) -> Area
defaultState    =  ($ Normal) . ($ NotYet)

--------------------------------------------------------------------------------
--  特殊用途パターンマッチング
--------------------------------------------------------------------------------
isNoAnime                                   :: Area -> Bool
isNoAnime       (Puyo _ _ Normal)           =  True
isNoAnime       (Ojama  _ Normal)           =  True
isNoAnime       _                           =  False

isDroppingAnime                             :: Area -> Bool
isDroppingAnime (Puyo _ _ (Dropping _ ))    =  True
isDroppingAnime (Ojama  _ (Dropping _ ))    =  True
isDroppingAnime _                           =  False


--------------------------------------------------------------------------------
--  特殊用途名前
--------------------------------------------------------------------------------
landPuyo        :: Color -> AnimationType -> Area
landPuyo c a    =  Puyo c NotYet a
