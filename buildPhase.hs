-- buildPhase.hs
module BuildPhase
    (
    build_playerPuyo,
    )
    where

import Control.Monad

import qualified PlayerState    as P
import QueryPS (
    get_nextPuyoColors,
    get_PlayerPuyoExistent,
    get_playerIdentity,
    )
import OverwritingPS   (
    eat_nextPuyo,
    renew_playerPuyo',
    renewScoreCalculation,
    toSupplyYokoku,
    toAdvanceYokoku,
    )

import qualified Typedata   as T (Color, Direction(DUp), AreaPosition)
import qualified Utility    as U (neighbor_area, againstTerritory)
import qualified Variable   as V 

--------------------------------------------------------------------------------
-- ����Ղ�𐶐�
--------------------------------------------------------------------------------
build_playerPuyo        :: V.GameState -> P.PlayerState -> IO()
build_playerPuyo gs state   =  do
    renewScoreCalculation (0*) (const []) (const []) state
    color   <- pick_nextPuyoColor state
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

-- �l�N�X�g�Ղ�̐F�𒲂ׂāA�l�N�X�g�Ղ惊�X�g�������B
pick_nextPuyoColor  :: P.PlayerState -> IO (T.Color, T.Color)
pick_nextPuyoColor state    = do
    colors  <- get_nextPuyoColors state
    let [cb, cm]  = take 2 colors
    eat_nextPuyo state 2
    return (cb, cm)