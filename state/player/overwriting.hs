module State.Player.Overwriting
( shift_gamePhase
, eat_nextPuyo
, renew_fieldArea
, renew_animationType
, renew_playerPuyo
, renew_playerPuyo'
, remove_playerPuyo
, renewScore
, renewYokoku
, renewLoseFlag
) where

import qualified State.Player.Substance as P'
import State.Player.Query

import Data.List    (nub)
import Data.Maybe   (fromMaybe)
import Control.Monad
import qualified Data.IORef         as IORF
import qualified Data.Array.IO      as AIO

import qualified Data.Setting  as V
import qualified Data.PlayerIdentity  as Identity
import qualified Data.Area            as Area
import qualified Data.Direction       as Direction
import Data.Time  (Time)
import qualified Data.Score           as Score
import Data.Color (Color)
import qualified Data.Field           as Field (Position)
import qualified Data.Yokoku          as Yokoku
import qualified Data.Number          as Number
import qualified Data.Phase           as Phase (Game)

--------------------------------------------------------------------------------
--  状態書き換え
--------------------------------------------------------------------------------
-- ゲーム状態を変える。
shift_gamePhase                 :: P'.PlayerState -> Phase.Game -> IO()
shift_gamePhase state gamephase =
    flip IORF.writeIORef gamephase $ P'.phase state

-- ネクストぷよを消費する。
eat_nextPuyo    :: P'.PlayerState -> Number.Puyo -> IO()
eat_nextPuyo state n =
    flip IORF.modifyIORef (drop n) $ P'.nexts state
    
--------------------------------------------------------------------------------
--  敗北フラグ更新
--------------------------------------------------------------------------------
-- 敗北フラグを更新する。
renewLoseFlag   :: Bool -> Identity.Territory -> P'.PlayerState -> IO()
renewLoseFlag b trt state   =  do
    loseFlag    <- IORF.readIORef refLoseFlag
    IORF.writeIORef refLoseFlag $ Identity.apply trt (const b) loseFlag
  where
    refLoseFlag = P'.loseFlag state

--------------------------------------------------------------------------------
--  予告ぷよ更新
--------------------------------------------------------------------------------
-- 予告ぷよを更新する。
renewYokoku :: (Yokoku.Shelf -> Yokoku.Shelf)
            -> Identity.Territory -> P'.PlayerState -> IO()
renewYokoku f trt state   = do
    yokoku <- IORF.readIORef $ P'.yokoku state
    IORF.writeIORef (P'.yokoku state) $ Identity.apply trt f yokoku

--------------------------------------------------------------------------------
--  得点更新
--------------------------------------------------------------------------------
-- 得点を書き換える。
renewScore  :: (Score.Score -> Score.Score) -> P'.PlayerState -> IO()
renewScore f state  =
    IORF.writeIORef (P'.score state) . f =<< get_score state

--------------------------------------------------------------------------------
--  フィールド状態更新
--------------------------------------------------------------------------------
-- エリアを更新する。
renew_fieldArea :: P'.PlayerState -> Field.Position -> Area.Area -> IO()
renew_fieldArea state p area    = AIO.writeArray (P'.field state) p area

-- アニメーション状態を更新する。
renew_animationType         :: P'.PlayerState -> Field.Position -> IO()
renew_animationType state p =  get_fieldStateArea p state >>=
                               AIO.writeArray (P'.field state) p . Area.countAT

--------------------------------------------------------------------------------
--  配ぷよ状態更新
--------------------------------------------------------------------------------
-- 部分書き換え
renew_playerPuyo :: P'.PlayerState 
                    -> Maybe (Color, Color)     -- （基点ぷよの色、動点ぷよの色）
                    -> Maybe Field.Position     -- 基点ぷよのフィールド座標
                    -> Maybe Direction.Area     -- 動点ぷよの方向
                    -> Maybe Time               -- 自然落下用のカウンタ
                    -> Maybe Time               -- 回転用のカウンタ
                    -> Maybe Bool               -- クイックターンフラグ
                    -> IO ()
renew_playerPuyo state c p d tf tt qf   = do
    P'.PlayerPuyoInfo c' p' d' tf' tt' qf'  <- IORF.readIORef stateP
    IORF.writeIORef stateP $ P'.PlayerPuyoInfo  (fromMaybe c'  c )
                                                (fromMaybe p'  p )
                                                (fromMaybe d'  d )
                                                (fromMaybe tf' tf)
                                                (fromMaybe tt' tt)
                                                (fromMaybe qf' qf)
  where stateP = P'.playerPuyo state

-- 完全書き換え
renew_playerPuyo'   :: P'.PlayerState 
                    -> (Color, Color)       -- （基点ぷよの色、動点ぷよの色）
                    -> Field.Position       -- 基点ぷよのフィールド座標
                    -> Direction.Area       -- 動点ぷよの方向
                    -> Time                 -- 自然落下用のカウンタ
                    -> Time                 -- 回転用のカウンタ
                    -> Bool                 -- クイックターンフラグ
                    -> IO ()
renew_playerPuyo' state c p d tf tt qf  =
  flip IORF.writeIORef (P'.PlayerPuyoInfo c p d tf tt qf) $ P'.playerPuyo state

-- 完全書き換え
remove_playerPuyo :: P'.PlayerState -> IO()
remove_playerPuyo =  flip IORF.writeIORef P'.NonExistent . P'.playerPuyo