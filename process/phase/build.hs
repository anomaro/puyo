-- buildPhase.hs
module Process.Phase.Build
    (
    build_playerPuyo,
    checkLose,
    )
    where

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
    renewScoreCalculation,
    toSupplyYokoku,
    toAdvanceYokoku,
    renewLoseFlag,
    )

import qualified Common.DataType   as T (Color, Direction(DUp), AreaPosition)
import qualified Common.Function   as U
import qualified State.Setting  as V 

import qualified Common.PlayerIdentity  as Identity
import qualified Common.Area            as Area     (isPuyo)

--------------------------------------------------------------------------------
--  ‘€ì‚Õ‚æ‚ð¶¬
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
    -- ‚Õ‚æ¶¬“_
    build_Area  = U.neighbor_area T.DUp $ V.criticalArea gs :: T.AreaPosition
    trt     = Identity.territory $ get_playerIdentity state

-- ƒlƒNƒXƒg‚Õ‚æ‚ÌF‚ð’²‚×‚ÄŽæ‚èo‚·B
pick_nextPuyoColor  :: P.PlayerState -> IO (T.Color, T.Color)
pick_nextPuyoColor state    = do
    colors  <- get_nextPuyoColors state
    eat_nextPuyo state 2
    return $ pair colors
  where
    pair (x:x':_)   = (x, x')

--------------------------------------------------------------------------------
--  ”s–k”»’è
--------------------------------------------------------------------------------
-- ”s–k”»’è i”s–k‚µ‚Ä‚¢‚½‚çA”s–kðŒ‚ð‘‚«Š·‚¦True‚ð•Ô‚·Bj
checkLose           :: V.GameState -> P.PlayerState -> IO Bool
checkLose gs state  =  do
    area <- get_fieldStateArea (V.criticalArea gs) state
    when (Area.isPuyo area) $ renewLoseFlag True trt state
    return $ Area.isPuyo area
  where
    trt     = Identity.territory $ get_playerIdentity state
