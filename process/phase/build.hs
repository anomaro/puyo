module Process.Phase.Build
( build_playerPuyo
, checkLose
) where

import Control.Monad (when)

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
    renewLoseFlag,
    )
import Data.Setting (Setting)
import qualified Data.PlayerIdentity  as Identity
import qualified Data.Area            as Area  (isPuyo)
import qualified Data.Direction       as Direction
import qualified Data.Score           as Score (refresh)
import Data.Color (Color)
import qualified Data.Field           as Field

--------------------------------------------------------------------------------
--  ����Ղ�𐶐�
--------------------------------------------------------------------------------
build_playerPuyo            :: Setting -> P.PlayerState -> IO()
build_playerPuyo gs state   =  do
    renewScore Score.refresh state
    color   <- pick_nextPuyoColor state
    eat_nextPuyo state 2
    renew_playerPuyo' state color build_Area Direction.Up 0 0 False
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
checkLose           :: Setting -> P.PlayerState -> IO Bool
checkLose gs state  =  do
    area <- get_fieldStateArea (Field.critical gs) state
    when (Area.isPuyo area) $ renewLoseFlag True trt state
    return $ Area.isPuyo area
  where
    trt     = Identity.territory $ get_playerIdentity state