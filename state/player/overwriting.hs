module State.Player.Overwriting
    (   
    -- ゲーム状態遷移更新
    shift_gamePhase,
    
    -- ネクストぷよ状態更新
    eat_nextPuyo,
    
    -- フィールド状態更新
    renew_fieldArea,
    renew_animationType,
    
    -- 配ぷよ状態更新
    renew_playerPuyo,
    renew_playerPuyo',
    remove_playerPuyo,
    
    -- 得点更新
    renewScore,

    -- 予告ぷよ更新
    renewYokoku,
    toSupplyYokoku,
    toAdvanceYokoku,
    
    -- 敗北フラグ更新
    renewLoseFlag,
    )
    where

import State.Player.Substance
import State.Player.Query

import Data.List    (nub)
import Data.Maybe   (fromMaybe)
import Control.Monad
import qualified Data.IORef         as IORF
import qualified Data.Array.IO      as AIO

import qualified Common.DataType   as T
import qualified Common.Function    as U
import qualified State.Setting  as V

import qualified Common.PlayerIdentity  as Identity
import qualified Common.Area            as Area
import qualified Common.Direction       as Direction
import Common.Time  (Time)
import qualified Common.Score           as Score

--------------------------------------------------------------------------------
--  状態書き換え
--------------------------------------------------------------------------------
-- ゲーム状態を変える。
shift_gamePhase                 :: PlayerState -> T.GamePhase -> IO()
shift_gamePhase state gamephase =
    flip IORF.writeIORef gamephase =<< takeout_gamePhaseState state

-- ネクストぷよを消費する。
eat_nextPuyo    :: PlayerState -> T.NumOfPuyo -> IO()
eat_nextPuyo state n =
    flip IORF.modifyIORef (drop n) =<< takeout_nextPuyoState state
    
--------------------------------------------------------------------------------
--  敗北フラグ更新
--------------------------------------------------------------------------------
-- 敗北フラグを更新する。
renewLoseFlag   :: Bool -> Identity.Territory -> PlayerState -> IO()
renewLoseFlag b trt state   =  do
    loseFlag    <- IORF.readIORef refLoseFlag
    IORF.writeIORef refLoseFlag $ Identity.apply trt (const b) loseFlag
  where
    refLoseFlag = takeout_loseFlagState state

--------------------------------------------------------------------------------
--  予告ぷよ更新
--------------------------------------------------------------------------------
-- 予告ぷよを更新する。
renewYokoku :: (T.NumOfPuyo -> T.NumOfPuyo)
            -> (T.NumOfPuyo -> T.NumOfPuyo)
            -> (T.NumOfPuyo -> T.NumOfPuyo)
            -> Identity.Territory -> PlayerState -> IO()
renewYokoku f1 f2 f3 trt state   = do
    yokokuField <- IORF.readIORef $ takeout_yokokuState state
    IORF.writeIORef (takeout_yokokuState state) $ Identity.apply trt f yokokuField
  where
    f (n1, n2, n3) = (fmap f1 n1, fmap f2 n2, fmap f3 n3)

-- 予告ぷよをReserveからSupplyへ移す。
toSupplyYokoku :: Identity.Territory -> PlayerState -> IO()
toSupplyYokoku trt state =  do
    yokokuField <- IORF.readIORef stateY
    IORF.writeIORef stateY $ Identity.apply trt shift yokokuField
  where
    stateY  = takeout_yokokuState state
    shift (n, Supply n2, Reserve n3)    = (n, Supply $ n2 + n3, Reserve 0)

-- 予告ぷよをSupplyからAdvanceへ移す。
toAdvanceYokoku :: T.NumOfPuyo -> Identity.Territory -> PlayerState -> IO()
toAdvanceYokoku m trt state =  do
    yokokuField <- IORF.readIORef stateY
    IORF.writeIORef stateY $ Identity.apply trt shift yokokuField
  where
    stateY  = takeout_yokokuState state
    shift (Advance n1, Supply n2, n)
        | n2 > m    = (Advance $ n1 + m , Supply $ n2 - m, n)
        | otherwise = (Advance $ n1 + n2, Supply 0       , n)

--------------------------------------------------------------------------------
--  得点更新
--------------------------------------------------------------------------------
-- 得点を書き換える。
renewScore  :: (Score.Score -> Score.Score) -> PlayerState -> IO()
renewScore f state  =
    IORF.writeIORef (takeout_scoreState state) . f =<< get_score state

--------------------------------------------------------------------------------
--  フィールド状態更新
--------------------------------------------------------------------------------
-- エリアを更新する。
renew_fieldArea :: PlayerState -> T.AreaPosition -> Area.Area -> IO()
renew_fieldArea state p area    =
    takeout_fieldstate state >>= \stateF -> AIO.writeArray stateF p area

-- アニメーション状態を更新する。
renew_animationType :: PlayerState -> T.AreaPosition -> IO()
renew_animationType state p =  do
    stateF  <- takeout_fieldstate state
    area    <- get_fieldStateArea p state
    AIO.writeArray stateF p $ Area.countAT area

--------------------------------------------------------------------------------
--  配ぷよ状態更新
--------------------------------------------------------------------------------
-- 部分書き換え
renew_playerPuyo :: PlayerState 
                    -> Maybe (T.Color, T.Color) -- （基点ぷよの色、動点ぷよの色）
                    -> Maybe T.AreaPosition     -- 基点ぷよのフィールド座標
                    -> Maybe Direction.Area     -- 動点ぷよの方向
                    -> Maybe Time               -- 自然落下用のカウンタ
                    -> Maybe Time               -- 回転用のカウンタ
                    -> Maybe Bool               -- クイックターンフラグ
                    -> IO ()
renew_playerPuyo state c p d tf tt qf   = do
    stateP                              <- takeout_ppuyostate state
    PlayerPuyoInfo c' p' d' tf' tt' qf' <- IORF.readIORef stateP
    IORF.writeIORef stateP $ PlayerPuyoInfo (fromMaybe c'  c )
                                            (fromMaybe p'  p )
                                            (fromMaybe d'  d )
                                            (fromMaybe tf' tf)
                                            (fromMaybe tt' tt)
                                            (fromMaybe qf' qf)

-- 完全書き換え
renew_playerPuyo'   :: PlayerState 
                    -> (T.Color, T.Color)   -- （基点ぷよの色、動点ぷよの色）
                    -> T.AreaPosition       -- 基点ぷよのフィールド座標
                    -> Direction.Area       -- 動点ぷよの方向
                    -> Time                 -- 自然落下用のカウンタ
                    -> Time                 -- 回転用のカウンタ
                    -> Bool                 -- クイックターンフラグ
                    -> IO ()
renew_playerPuyo' state c p d tf tt qf  =
    flip IORF.writeIORef (PlayerPuyoInfo c p d tf tt qf) 
                                        =<< takeout_ppuyostate state

-- 完全書き換え
remove_playerPuyo :: PlayerState -> IO()
remove_playerPuyo =  flip IORF.writeIORef NonExistent <=< takeout_ppuyostate