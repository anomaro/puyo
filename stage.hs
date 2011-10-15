module Stage
where

import qualified State.Player.DataType  as P
import qualified State.Player.Query     as Q 
import qualified State.Configuration    as CT
import qualified State.Result           as D

import State.Setting (Setting)
import qualified Common.PlayerIdentity  as Identity

--------------------------------------------------------------------------------
--  ê–Ê
--------------------------------------------------------------------------------
data GameStage
    = Game GameName Setting PlayerStates D.GameDataCollection
    | Configuration Setting CT.Selection CT.ConfiguPhase

data GameName   = Puyopuyo  -- ’Êí‚Ì‚Õ‚æ‚Õ‚æ

type PlayerStates   = (P.PlayerState, P.PlayerState)

--------------------------------------------------------------------------------
--  ê–Êì¬
--------------------------------------------------------------------------------
createGameStage         :: Setting -> D.GameDataCollection -> IO GameStage
createGameStage gs gdc  =  do
    n1P         <- Q.create_nextPuyoState gs
    n2P         <- Q.copy_nextPuyoState n1P
    y           <- Q.create_yokokuState
    l           <- Q.create_loseFlagState
    state1P     <- Q.create_playerstate Identity.defaultUser1P gs n1P y l
    state2P     <- Q.create_playerstate Identity.defaultUser2P gs n2P y l
    return $ Game Puyopuyo gs (state1P, state2P) gdc

createConfigurationStage    :: Setting -> IO GameStage
createConfigurationStage gs =  
    return $ Configuration gs CT.initialEntry CT.SelectPhase