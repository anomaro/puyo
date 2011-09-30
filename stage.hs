-- stage.hs
module Stage
    where

--import qualified Typedata       as T
import qualified State.Player.DataType  as P
import qualified State.Player.Query     as Q 
import qualified State.Configuration  as CT
import qualified State.Result     as D

import qualified State.Setting   as V
import qualified Common.Name     as W

import qualified Common.PlayerIdentity  as Identity

--------------------------------------------------------------------------------
--  場面
--------------------------------------------------------------------------------
data GameStage
    = Game GameName V.GameState PlayerStates D.GameDataCollection
    | Configuration V.GameState CT.Selection CT.ConfiguPhase

data GameName   = Puyopuyo  -- 通常のぷよぷよ

type PlayerStates   = (P.PlayerState, P.PlayerState)

--------------------------------------------------------------------------------
--  場面作成
--------------------------------------------------------------------------------
createGameStage         :: V.GameState -> D.GameDataCollection -> IO GameStage
createGameStage gs gdc  =  do
    n1P         <- Q.create_nextPuyoState gs
    n2P         <- Q.copy_nextPuyoState n1P
    y           <- Q.create_yokokuState
    l           <- Q.create_loseFlagState
    state1P     <- Q.create_playerstate Identity.defaultUser1P gs n1P y l
    state2P     <- Q.create_playerstate Identity.defaultUser2P gs n2P y l
    return $ Game Puyopuyo gs (state1P, state2P) gdc

createConfigurationStage    :: V.GameState -> IO GameStage
createConfigurationStage gs =  
    return $ Configuration gs CT.initialEntry CT.SelectPhase