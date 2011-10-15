module Render.Configuration
where

import qualified Graphics.UI.GLUT   as GLUT

import State.Setting  (Setting)
import Render.Object
import State.Configuration

--------------------------------------------------------------------------------
--  設定画面描画
--------------------------------------------------------------------------------
renderConfiguration         :: Setting -> Selection -> IO ()
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
