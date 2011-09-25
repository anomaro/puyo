-- controlPhase.hs
module ControlPhase
    (
    fallNatural_playerPuyo,
    fall_puyo,
    move_puyo,
    rotate_puyo,
    )
    where

import qualified Control.Monad      as MND

import qualified Typedata   as T (Time, Direction(..), GamePhase(DropPhase),
                                  RotationDirection(..) )
import qualified Utility    as U (neighbor_area, rotate_direction)
import qualified Variable   as V (GameState, get, GameStateIndex(FallTime))
import qualified World      as W (animeTime_rotate, flag_quickTrun)

import PlayerState
import QueryPS (
    get_PlayerPuyoExistent,
    get_PlayerPuyoPosition,
    get_PlayerPuyoDirection,
    get_PlayerPuyoFallTime,
    get_PlayerPuyoRotateTime,
    get_PlayerPuyoQuickTurnFlag,
    is_neighborSpace,
    get_PlayerPuyoColors,
    )
import OverwritingPS   (
    renew_playerPuyo,
    shift_gamePhase,
    )

--------------------------------------------------------------------------------
-- 自然落下処理。（移動した場合はTrueを返す。）
--------------------------------------------------------------------------------
fallNatural_playerPuyo :: PlayerState -> V.GameState -> IO Bool
fallNatural_playerPuyo state gs = do
    timeF   <- get_PlayerPuyoFallTime state
    if timeF > 0
      then count timeF          >>  return False
      else fall_puyo state gs
  where
    -- カウンタを進める。
    count       :: T.Time -> IO()
    count timeF =  do
        timeR   <- get_PlayerPuyoRotateTime state
        renew_playerPuyo state Nothing
                               Nothing
                               Nothing
                               (Just $ timeF - 1)
                               (Just $ timeR - signum timeR)
                               Nothing

--------------------------------------------------------------------------------
-- 操作ぷよの落下移動。（移動した場合はTrueを返す。）
--------------------------------------------------------------------------------
fall_puyo           :: PlayerState -> V.GameState -> IO Bool
fall_puyo state gs  =  reset_fallTime >> move_puyo state T.DDown
  where
    -- カウンタをリセットする。
    reset_fallTime :: IO()
    reset_fallTime =  do
        b   <- get_PlayerPuyoExistent state
        MND.when b $
            renew_playerPuyo state Nothing
                                   Nothing
                                   Nothing
                                   (Just $ V.get V.FallTime gs)
                                   Nothing
                                   Nothing

--------------------------------------------------------------------------------
-- 操作ぷよの移動。（移動した場合はTrueを返す。）
--------------------------------------------------------------------------------
move_puyo :: PlayerState -> T.Direction -> IO Bool
move_puyo state dm  =  do
    flagExistent    <- get_PlayerPuyoExistent state
    if flagExistent then move_puyo' else return False
  where            
    move_puyo' :: IO Bool
    move_puyo' =  do
        move    <- is_move state dm
        pos     <- get_PlayerPuyoPosition state
        if move         -- 移動判定 --
          then move_neighbor pos
          else MND.when (dm == T.DDown)   -- 着地判定 --
                    $ (shift_gamePhase state T.DropPhase) >> land_puyo
        return move
      where
        move_neighbor p = renew_playerPuyo state Nothing
                                                 (Just $ U.neighbor_area dm p)
                                                 Nothing
                                                 Nothing
                                                 Nothing
                                                 Nothing
        land_puyo   = renew_playerPuyo state Nothing
                                             Nothing
                                             Nothing
                                             (Just 0)
                                             (Just 0)
                                             (Just False)

--------------------------------------------------------------------------------
-- 操作ぷよの回転。（移動した場合はTrueを返す。 ※回転した場合ではない。）
--------------------------------------------------------------------------------
rotate_puyo :: PlayerState -> T.RotationDirection -> IO(Bool)
rotate_puyo state rd    = do
    (y, x)  <- get_PlayerPuyoPosition       state
    d       <- get_PlayerPuyoDirection      state
    tt      <- get_PlayerPuyoRotateTime     state
    b       <- get_PlayerPuyoQuickTurnFlag  state
    if tt /= 0
      then  return False
      else  do 
        let d90     = U.rotate_direction rd d
            d180    = U.rotate_direction rd d90
            d270    = U.rotate_direction rd d180
        isSpace90   <- is_neighborSpace (y, x) d90  state
        isSpace180  <- is_neighborSpace (y, x) d180 state
        isSpace270  <- is_neighborSpace (y, x) d270 state
        
        case ( b, (isSpace90, isSpace180, isSpace270) )
         of (_, (True , _, _))  -> rotate d90 rd    >> return False
            (_, (_, _, True ))  -> rotate d90 rd    >> move_puyo state d270
            (False, _)          -> permit_quickTurn >> return False
            (_, (_, True, _))   -> rotate d180 rd   >> return False
            _                   -> rotate d180 rd   >> move_puyo state d
  where
    -- 回転後の状態へ書き換える。
    rotate :: T.Direction -> T.RotationDirection -> IO()
    rotate d rd = do
        let newRT = if rd == T.RLeft then (-W.animeTime_rotate)
                                     else W.animeTime_rotate
        renew_playerPuyo state Nothing
                               Nothing
                               (Just d)
                               Nothing
                               (Just newRT)
                               (Just False)
    -- クイックターンの許可。
    permit_quickTurn :: IO()
    permit_quickTurn =  do
        renew_playerPuyo state Nothing
                               Nothing
                               Nothing
                               Nothing
                               Nothing
                               (Just W.flag_quickTrun)

--------------------------------------------------------------------------------
-- 移動できるかどうか判定。（隣接するマスが空白かどうか調べる。）
is_move :: PlayerState -> T.Direction -> IO Bool
is_move state dm = do
    pos <- get_PlayerPuyoPosition  state
    dp  <- get_PlayerPuyoDirection state
    result_basePuyo <- is_neighborSpace pos dm state
    if result_basePuyo
      then is_neighborSpace (U.neighbor_area dp pos) dm state
      else return False