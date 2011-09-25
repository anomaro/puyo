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
-- 操作ぷよを生成
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
    -- ぷよ生成点
    build_Area  = U.neighbor_area T.DUp $ V.criticalArea gs :: T.AreaPosition
    trt     = fst $ get_playerIdentity state

-- ネクストぷよの色を調べて取り出す。
pick_nextPuyoColor  :: P.PlayerState -> IO (T.Color, T.Color)
pick_nextPuyoColor state    = do
    colors  <- get_nextPuyoColors state
    return $ pair colors
  where
    pair (x:x':_)   = (x, x')