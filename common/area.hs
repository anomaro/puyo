module Common.Area
( Area
, color
, anime

, UnionCheck
, AnimationType
, Power

, initialFst
, initialSnd
, defaultOjamaPuyo
, animeStartDropping
, animeStartErasing
, animeStartLanding
, defauletPower

, isPuyo
, is_colorPuyo
, isSpace
, modifyColor
, modifyUnion
, modifyAnime
, countAT

, isNoAnime
, isDroppingAnime

, eracingPuyo

, isUnionCheck
, isUnionCheckFinished
, isReplacedSpace
, isEraseOjamaPuyo
, unionCheckCompletion

, isLink
, morph

, landPuyo
)
where

import Data.Maybe (fromJust)

import Standardizable
import Common.DataType
import Common.Name
import qualified State.Setting  as V

import qualified Common.Name      as W

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
instance Standardizable Area where
    standard        = Space

-- ぷよの結合チェック状態。
data UnionCheck = NotYet        -- 未調査
                | Completion    -- 調査完了
                | EraseFlag     -- 消滅フラグ
        deriving (Show, Eq)
instance Standardizable UnionCheck where
    standard        = NotYet

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
eracingPuyo             :: Maybe Color -> Area
eracingPuyo Nothing     =  Ojama  EraseFlag animeStartErasing
eracingPuyo (Just c)    =  Puyo c EraseFlag animeStartErasing

--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
isUnionCheck            :: Maybe Color -> Area -> AreaPosition -> Bool
isUnionCheck            =  isTarget NotYet

isUnionCheckFinished    :: Maybe Color -> Area -> AreaPosition -> Bool
isUnionCheckFinished    =  isTarget Completion

isReplacedSpace         :: Area -> AreaPosition -> Bool
isReplacedSpace         =  isTarget EraseFlag Nothing

-- 対象のエリアが結合チェック・消滅の対象かどうか判定。
isTarget :: UnionCheck -> Maybe Color -> Area -> AreaPosition -> Bool
isTarget u c (Puyo c' u' _) (y, _) | y >= V.topFieldRank && u' == u
        = c == Nothing || c' == fromJust c
isTarget EraseFlag Nothing (Ojama EraseFlag _)  (y, _)  = y >= V.topFieldRank
isTarget _ _ _ _                                        = False

isEraseOjamaPuyo                    :: Area -> Bool
isEraseOjamaPuyo (Ojama NotYet _)   =  True
isEraseOjamaPuyo _                  =  False

unionCheckCompletion    :: Area -> Area
unionCheckCompletion    =  modifyUnion (const Completion)

--------------------------------------------------------------------------------
--  render
--------------------------------------------------------------------------------
isLink                          :: Area -> Bool
isLink (Puyo _ u Normal)        =  u == NotYet  || u == EraseFlag
isLink (Puyo _ u (Erasing _))   =  u == NotYet  || u == EraseFlag
isLink _                        =  False

type  PartialMorph a = Double -> Double -> Double -> Double -> Double -> a
morph       :: AnimationType -> PositionY -> Maybe ( PartialMorph a  -> a )
morph (Landing t pow) height    
    = Just (($ 0) . ($ scaleY) . ($ 1.2 * scaleX) . ($ moveY) . ($ 0))
      where
        moveY   = - 1 / (scaleY * 4) * ( pow' )
        scaleY  = recip scaleX
        scaleX  = (sqrt $ 1.01 * power - 1) * timeCoefficient + 1
          where
            power           = 1.1 + 0.1 * (pow' - 1)
            timeCoefficient = ((halfTime - remTime) / halfTime) ^ 2 -- 時間係数
              where 
                halfTime    = fromIntegral W.amimeTime_land / 2 + 1
                remTime     = abs $ halfTime - fromIntegral t   -- 残り時間
        pow'    | height == 0 && pow >= defauletPower   = pow - 1
                | otherwise                             = pow
morph (Erasing t)     _
    | t `rem` 8 >= 4            = Nothing
morph (Dropping t)    _         = Just (($ 0) . ($ 1) . ($ 1) . ($ mY) . ($ 0))
    where mY = fromIntegral W.amimeTime_drop * 1 / fromIntegral W.amimeTime_drop
morph _ _                       = Just (($ 0) . ($ 1) . ($ 1) . ($ 0) . ($ 0))

--------------------------------------------------------------------------------
--  drop
--------------------------------------------------------------------------------
landPuyo    :: Color -> AreaPosition -> Direction -> Bool -> (Area, Power)
landPuyo col pos@(y, _) dir isNeighborSpace = (Puyo col NotYet anime, power)
  where
    anime   = (animeStartLanding power) `orStandardIf` not (isNeighborSpace && dir /= DDown)
    power   = if dir == DUp then defauletPower - 1 else defauletPower