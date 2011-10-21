-- controlPhase.hs
module Process.Phase.Control
( fallNatural_playerPuyo
, fall_puyo
, move_puyo
, rotate_puyo
) where

import qualified Control.Monad      as MND

import State.Player.DataType
import State.Player.Query   (
    get_PlayerPuyoExistent,
    get_PlayerPuyoPosition,
    get_PlayerPuyoDirection,
    get_PlayerPuyoFallTime,
    get_PlayerPuyoRotateTime,
    get_PlayerPuyoQuickTurnFlag,
    is_neighborSpace,
    get_PlayerPuyoColors,
    )
import State.Player.Overwriting (
    renew_playerPuyo,
    shift_gamePhase,
    )
import qualified Data.Setting          as Setting
import qualified Data.Direction       as Direction
import qualified Data.Time            as Time (Time, animeRotate, count)
import qualified Data.Field           as Field (neighbor)
import qualified Data.Phase           as Phase (Game(Drop))

--------------------------------------------------------------------------------
-- 自然落下処理。（移動した場合はTrueを返す。）
--------------------------------------------------------------------------------
fallNatural_playerPuyo :: PlayerState -> Setting.Setting -> IO Bool
fallNatural_playerPuyo state gs = do
    timeF   <- get_PlayerPuyoFallTime state
    if timeF > 0
      then count timeF          >>  return False
      else fall_puyo state gs
  where
    -- カウンタを進める。
    count       :: Time.Time -> IO()
    count timeF =  do
        timeR   <- get_PlayerPuyoRotateTime state
        renew_playerPuyo state Nothing
                               Nothing
                               Nothing
                               (Just $ Time.count timeF)
                               (Just $ Time.count timeR)
                               Nothing

--------------------------------------------------------------------------------
-- 操作ぷよの落下移動。（移動した場合はTrueを返す。）
--------------------------------------------------------------------------------
fall_puyo           :: PlayerState -> Setting.Setting -> IO Bool
fall_puyo state gs  =  reset_fallTime >> move_puyo state Direction.Down
  where
    -- カウンタをリセットする。
    reset_fallTime :: IO()
    reset_fallTime =  do
        b   <- get_PlayerPuyoExistent state
        MND.when b $
            renew_playerPuyo state Nothing
                                   Nothing
                                   Nothing
                                   (Just $ Setting.get Setting.FallTime gs)
                                   Nothing
                                   Nothing

--------------------------------------------------------------------------------
-- 操作ぷよの移動。（移動した場合はTrueを返す。）
--------------------------------------------------------------------------------
move_puyo :: PlayerState -> Direction.Area -> IO Bool
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
          else MND.when (dm == Direction.Down)   -- 着地判定 --
                    $ (shift_gamePhase state Phase.Drop) >> land_puyo
        return move
      where
        move_neighbor p = renew_playerPuyo state Nothing
                                                 (Just $ Field.neighbor dm p)
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
rotate_puyo :: PlayerState -> Direction.Rotation -> IO(Bool)
rotate_puyo state rd    = do
    (y, x)  <- get_PlayerPuyoPosition       state
    d       <- get_PlayerPuyoDirection      state
    tt      <- get_PlayerPuyoRotateTime     state
    b       <- get_PlayerPuyoQuickTurnFlag  state
    if tt /= 0
      then  return False
      else  do 
        let d90     = Direction.rotate rd d
            d180    = Direction.rotate rd d90
            d270    = Direction.rotate rd d180
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
    rotate :: Direction.Area -> Direction.Rotation -> IO()
    rotate d rd = do
        let 
        renew_playerPuyo state Nothing
                               Nothing
                               (Just d)
                               Nothing
                               (Just $ f Time.animeRotate)
                               (Just False)
      where f = if rd == Direction.Clockwise    then id     else negate
    -- クイックターンの許可。
    permit_quickTurn :: IO()
    permit_quickTurn =  do
        renew_playerPuyo state Nothing
                               Nothing
                               Nothing
                               Nothing
                               Nothing
                               (Just Setting.flag_quickTrun)

--------------------------------------------------------------------------------
-- 移動できるかどうか判定。（隣接するマスが空白かどうか調べる。）
is_move :: PlayerState -> Direction.Area -> IO Bool
is_move state dm = do
    pos <- get_PlayerPuyoPosition  state
    dp  <- get_PlayerPuyoDirection state
    result_basePuyo <- is_neighborSpace pos dm state
    if result_basePuyo
      then is_neighborSpace (Field.neighbor dp pos) dm state
      else return False