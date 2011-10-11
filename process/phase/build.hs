-- buildPhase.hs
module Process.Phase.Build
( build_playerPuyo
, checkLose
) where

import Control.Monad

import State.Player.DataType    as P
import State.Player.Query   (
    get_nextPuyoColors,
    get_PlayerPuyoExistent,
    get_playerIdentity,
    get_fieldStateArea,
    )
import State.Player.Overwriting (
    eat_nextPuyo,
    renew_playerPuyo',
    renewScore,
    toSupplyYokoku,
    toAdvanceYokoku,
    renewLoseFlag,
    )

import qualified Common.DataType   as T
import qualified Common.Function   as U
import qualified State.Setting  as V 

import qualified Common.PlayerIdentity  as Identity
import qualified Common.Area            as Area  (isPuyo)
import qualified Common.Direction       as Direction
import qualified Common.Score           as Score (refresh)
import Common.Color (Color)
import qualified Common.Field           as Field

--------------------------------------------------------------------------------
--  ����Ղ�𐶐�
--------------------------------------------------------------------------------
build_playerPuyo        :: V.GameState -> P.PlayerState -> IO()
build_playerPuyo gs state   =  do
    renewScore Score.refresh state
    color   <- pick_nextPuyoColor state
    eat_nextPuyo state 2
    renew_playerPuyo' state color
                            build_Area
                            Direction.Up
                            0
                            0
                            False
  where
    -- �Ղ搶���_
    build_Area  = Field.neighbor Direction.Up $ Field.critical gs
    trt     = Identity.territory $ get_playerIdentity state

-- �l�N�X�g�Ղ�̐F�𒲂ׂĎ��o���B
pick_nextPuyoColor  :: P.PlayerState -> IO (Color, Color)
pick_nextPuyoColor state    = do
    colors  <- get_nextPuyoColors state
    eat_nextPuyo state 2
    return $ pair colors
  where
    pair (x:x':_)   = (x, x')

--------------------------------------------------------------------------------
--  �s�k����
--------------------------------------------------------------------------------
-- �s�k���� �i�s�k���Ă�����A�s�k��������������True��Ԃ��B�j
checkLose           :: V.GameState -> P.PlayerState -> IO Bool
checkLose gs state  =  do
    area <- get_fieldStateArea (Field.critical gs) state
    when (Area.isPuyo area) $ renewLoseFlag True trt state
    return $ Area.isPuyo area
  where
    trt     = Identity.territory $ get_playerIdentity state