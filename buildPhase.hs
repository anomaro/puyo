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
--  ìÕæð¶¬
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
    -- Õæ¶¬_
    build_Area  = U.neighbor_area T.DUp $ V.criticalArea gs :: T.AreaPosition
    trt     = fst $ get_playerIdentity state

-- lNXgÕæÌFð²×ÄAlNXgÕæXgðÁï·éB
pick_nextPuyoColor  :: P.PlayerState -> IO (T.Color, T.Color)
pick_nextPuyoColor state    = do
    colors  <- get_nextPuyoColors state
    let [cb, cm]  = take 2 colors
    eat_nextPuyo state 2
    return (cb, cm)

--------------------------------------------------------------------------------
--  sk»è
--------------------------------------------------------------------------------
-- sk»è iskµÄ¢½çAskðð«·¦TrueðÔ·Bj
checkLose           :: V.GameState -> P.PlayerState -> IO Bool
checkLose gs state  =  do
    area <- get_fieldStateArea (V.criticalArea gs) state
    when (U.isPuyo area) $ renewLoseFlag True trt state
    return $ U.isPuyo area
  where
    trt     = fst $ get_playerIdentity state