-- configuration.hs
module Process.Configuration
    where

import Data.Maybe

import qualified Input      as I
import qualified State.Setting   as Setting (renew)
import qualified Stage      as S
import qualified State.Result   as D

import State.Configuration

--------------------------------------------------------------------------------
--  ƒL[“ü—Í‰ðŽß
--------------------------------------------------------------------------------
convertConfigurationPhase   :: I.ButtonState -> S.GameStage -> IO S.GameStage
convertConfigurationPhase bs stage  = do
    buttons <- I.read_buttonState bs
    convertConfigurationPhase' buttons stage

convertConfigurationPhase'  :: [I.Button] -> S.GameStage -> IO S.GameStage
convertConfigurationPhase' bs stage@(S.Configuration gs sl SelectPhase)
  | elem I.one_button   bs  = S.createGameStage gs D.initialGameDataCollection
  | elem I.up_button    bs  = retCon gs (predSelection sl) nextPhase
  | elem I.down_button  bs  = retCon gs (succSelection sl) nextPhase
  | elem I.right_button bs  = retCon (gs' succ) sl nextPhase
  | elem I.left_button  bs  = retCon (gs' pred) sl nextPhase
  | otherwise               = return stage
  where
    nextPhase   = defaultConfigAnimePhase
    retCon a b c    = return $ S.Configuration a b c 
    gs' f   | isJust sl'    = Setting.renew f (fromJust $ sl') gs
            | otherwise     = gs
    sl'     = toGameStateIndex sl
convertConfigurationPhase' bs (S.Configuration gs sl (AnimationPhase 0))    =
    return $ S.Configuration gs sl SelectPhase
convertConfigurationPhase' bs (S.Configuration gs sl (AnimationPhase n))    =
    return $ S.Configuration gs sl (AnimationPhase $ n - 1)