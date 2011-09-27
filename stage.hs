-- stage.hs
module Stage
    where

--import qualified Typedata       as T
import qualified PlayerState    as P
import qualified QueryPS        as Q 
import qualified ConfigurationTypeData  as CT
import qualified GameDataCollection     as D

import qualified Variable   as V
import qualified World      as W

--------------------------------------------------------------------------------
--  ê–Ê
--------------------------------------------------------------------------------
data GameStage
    = Game GameName V.GameState PlayerStates D.GameDataCollection
    | Configuration V.GameState CT.Selection CT.ConfiguPhase

data GameName   = Puyopuyo  -- ’Êí‚Ì‚Õ‚æ‚Õ‚æ

type PlayerStates   = (P.PlayerState, P.PlayerState)

--------------------------------------------------------------------------------
--  ê–Êì¬
--------------------------------------------------------------------------------
createGameStage     :: V.GameState -> IO GameStage
createGameStage gs  =  do
    n1P         <- Q.create_nextPuyoState gs
    n2P         <- Q.copy_nextPuyoState n1P
    y           <- Q.create_yokokuState
    l           <- Q.create_loseFlagState
    state1P     <- Q.create_playerstate W.defaultUser1P gs n1P y l
    state2P     <- Q.create_playerstate W.defaultUser2P gs n2P y l
    return $ Game Puyopuyo gs (state1P, state2P) D.initialGameDataCollection

createConfigurationStage    :: V.GameState -> IO GameStage
createConfigurationStage gs =  
    return $ Configuration gs CT.initialEntry CT.SelectPhase