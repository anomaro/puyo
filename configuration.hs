-- configuration.hs
module Configuration
    where

import Data.Maybe
import qualified Graphics.UI.GLUT   as GLUT

--import PlayerState
import qualified Input      as I
import qualified Variable   as V
import qualified Stage      as S

import RenderObject
import ConfigurationTypeData

--------------------------------------------------------------------------------
--  設定画面描画
--------------------------------------------------------------------------------
renderConfiguration         :: V.GameState -> Selection -> IO ()
renderConfiguration gs sl   =  do
    GLUT.lineWidth GLUT.$= 3.0
    GLUT.color (GLUT.Color3 1.0 1.0 1.0 :: GLUT.Color3 Double)
    renderTitle      "Configuration"
    mapM_ (renderEntry gs sl) listEntry
  where
    scaleTitle      = 0.002
    scale           = 0.001
    linesInterval   = 0.145
    staringEntryPointY  = 0.55
    
    -- 画面タイトル表示
    renderTitle txt = do
        fontSizeX   <- GLUT.stringWidth GLUT.Roman txt
        let xm  = - fromIntegral fontSizeX / 2 * scaleTitle
        render_gameobject' (GLUT.Vector3 xm 0.75 0) scaleTitle scaleTitle 0 
            $ GLUT.renderString GLUT.Roman txt

    -- 項目表示
    renderEntry gs sl e = do
        changeColor
        render_gameobject' (GLUT.Vector3 xm  ym 0) scale scale 0 
                                        $ GLUT.renderString GLUT.Roman txt
        render_gameobject' (GLUT.Vector3 xm' ym 0) scale scale 0 
                                        $ GLUT.renderString GLUT.Roman ntxt
      where
        ym  = - fromIntegral (fromEnum e) * linesInterval + staringEntryPointY
        xm  = -0.9
        xm' = xm + 1.1
        txt     = toEntryName e
        ntxt    = getGameStateValue e gs
        changeColor
          | sl == e     = GLUT.color (GLUT.Color3 1 1 0 :: GLUT.Color3 Double)
          | otherwise   = GLUT.color (GLUT.Color3 1 1 1 :: GLUT.Color3 Double)

--------------------------------------------------------------------------------
--  キー入力解釈
--------------------------------------------------------------------------------
convertConfigurationPhase   :: I.ButtonState -> S.GameStage -> IO S.GameStage
convertConfigurationPhase bs stage  = do
    buttons <- I.read_buttonState bs
    convertConfigurationPhase' buttons stage

convertConfigurationPhase'  :: [I.Button] -> S.GameStage -> IO S.GameStage
convertConfigurationPhase' bs stage@(S.Configuration gs sl SelectPhase)
  | elem I.one_button   bs = S.createGameStage gs
  | elem I.up_button    bs = retCon gs (predSelection sl) nextPhase
  | elem I.down_button  bs = retCon gs (succSelection sl) nextPhase
  | elem I.right_button bs = retCon (gs' succ) sl nextPhase
  | elem I.left_button  bs = retCon (gs' pred) sl nextPhase
  | otherwise               = return stage
  where
    nextPhase   = defaultConfigAnimePhase
    retCon a b c    = return $ S.Configuration a b c 
    gs' f   | isJust sl'    = V.newGameState f (fromJust $ sl') gs
            | otherwise     = gs
    sl'     = toGameStateIndex sl
convertConfigurationPhase' bs (S.Configuration gs sl (AnimationPhase 0))    =
    return $ S.Configuration gs sl SelectPhase
convertConfigurationPhase' bs (S.Configuration gs sl (AnimationPhase n))    =
    return $ S.Configuration gs sl (AnimationPhase $ n - 1)