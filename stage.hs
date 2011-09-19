-- stage.hs
module Stage
    where

--import qualified Typedata       as T
import qualified PlayerState    as P
import qualified QueryPS        as Q 
import qualified ConfigurationTypeData  as CT

import qualified Variable   as V
import qualified World      as W

--------------------------------------------------------------------------------
--  ê–Ê
--------------------------------------------------------------------------------
--data GameStage  = Game GameName (P.PlayerState, P.PlayerState)  -- ƒQ[ƒ€i‚Õ‚æ‚Õ‚æj
data GameStage  = Game GameName V.GameState (P.PlayerState, P.PlayerState)
                | Configuration V.GameState CT.Selection CT.ConfiguPhase

data GameName   = Puyopuyo  -- ’Êí‚Ì‚Õ‚æ‚Õ‚æ

--------------------------------------------------------------------------------
--  ê–Êì¬
--------------------------------------------------------------------------------
createGameStage     :: V.GameState -> IO GameStage
createGameStage gs  =  do
    stateN1P        <- Q.create_nextPuyoState gs
    stateN2P        <- Q.copy_nextPuyoState stateN1P
    stateY          <- Q.create_yokokuState
    plyaerstate1P   <- Q.create_playerstate W.defaultUser1P gs stateN1P stateY
    plyaerstate2P   <- Q.create_playerstate W.defaultUser2P gs stateN2P stateY
    return $ Game Puyopuyo gs (plyaerstate1P, plyaerstate2P)

createConfigurationStage    :: V.GameState -> IO GameStage
createConfigurationStage gs =  
    return $ Configuration gs CT.initialEntry CT.SelectPhase

