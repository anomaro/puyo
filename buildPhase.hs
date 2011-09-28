-- buildPhase.hs
module BuildPhase
    (
    build_playerPuyo,
    checkLose,
    )
    where

import Control.Monad

import qualified PlayerState    as P
import QueryPS (
    get_nextPuyoColors,
    get_PlayerPuyoExistent,
    get_playerIdentity,
    get_fieldStateArea,
    )
import OverwritingPS   (
    eat_nextPuyo,
    renew_playerPuyo',
    renewScoreCalculation,
    toSupplyYokoku,
    toAdvanceYokoku,
    renewLoseFlag,
    )

import qualified Typedata   as T (Color, Direction(DUp), AreaPosition)
import qualified Utility    as U (neighbor_area, againstTerritory, isPuyo)
import qualified Variable   as V 

--------------------------------------------------------------------------------
--  ����Ղ�𐶐�
--------------------------------------------------------------------------------
build_playerPuyo        :: V.GameState -> P.PlayerState -> IO()
build_playerPuyo gs state   =  do
    renewScoreCalculation (0*) (const []) (const []) state
    color   <- pick_nextPuyoColor state
    eat_nextPuyo state 2
    renew_playerPuyo' state color
                            build_Area
                            T.DUp
                            0
                            0
                            False
  where
    -- �Ղ搶���_
    build_Area  = U.neighbor_area T.DUp $ V.criticalArea gs :: T.AreaPosition
    trt     = fst $ get_playerIdentity state

-- �l�N�X�g�Ղ�̐F�𒲂ׂĎ��o���B
pick_nextPuyoColor  :: P.PlayerState -> IO (T.Color, T.Color)
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
    area <- get_fieldStateArea (V.criticalArea gs) state
    when (U.isPuyo area) $ renewLoseFlag True trt state
    return $ U.isPuyo area
  where
    trt     = fst $ get_playerIdentity state
