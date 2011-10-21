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
-- ���R���������B�i�ړ������ꍇ��True��Ԃ��B�j
--------------------------------------------------------------------------------
fallNatural_playerPuyo :: PlayerState -> Setting.Setting -> IO Bool
fallNatural_playerPuyo state gs = do
    timeF   <- get_PlayerPuyoFallTime state
    if timeF > 0
      then count timeF          >>  return False
      else fall_puyo state gs
  where
    -- �J�E���^��i�߂�B
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
-- ����Ղ�̗����ړ��B�i�ړ������ꍇ��True��Ԃ��B�j
--------------------------------------------------------------------------------
fall_puyo           :: PlayerState -> Setting.Setting -> IO Bool
fall_puyo state gs  =  reset_fallTime >> move_puyo state Direction.Down
  where
    -- �J�E���^�����Z�b�g����B
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
-- ����Ղ�̈ړ��B�i�ړ������ꍇ��True��Ԃ��B�j
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
        if move         -- �ړ����� --
          then move_neighbor pos
          else MND.when (dm == Direction.Down)   -- ���n���� --
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
-- ����Ղ�̉�]�B�i�ړ������ꍇ��True��Ԃ��B ����]�����ꍇ�ł͂Ȃ��B�j
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
    -- ��]��̏�Ԃ֏���������B
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
    -- �N�C�b�N�^�[���̋��B
    permit_quickTurn :: IO()
    permit_quickTurn =  do
        renew_playerPuyo state Nothing
                               Nothing
                               Nothing
                               Nothing
                               Nothing
                               (Just Setting.flag_quickTrun)

--------------------------------------------------------------------------------
-- �ړ��ł��邩�ǂ�������B�i�אڂ���}�X���󔒂��ǂ������ׂ�B�j
is_move :: PlayerState -> Direction.Area -> IO Bool
is_move state dm = do
    pos <- get_PlayerPuyoPosition  state
    dp  <- get_PlayerPuyoDirection state
    result_basePuyo <- is_neighborSpace pos dm state
    if result_basePuyo
      then is_neighborSpace (Field.neighbor dp pos) dm state
      else return False