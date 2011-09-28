-- main.hs
module Main
    (
    main
    )
    where

import Control.Applicative
import qualified Graphics.UI.GLUT   as GLUT 
import qualified System.Exit        as EXIT (ExitCode(ExitSuccess), exitWith)

import qualified Variable   as V (GameState, initialGameState)
import qualified World      as W -- (frame_rate, defaultUser1P, defaultUser2P)

import qualified GamePhase      as G
import qualified Render         as R
import qualified Input          as I (
    ButtonState,
    create_buttonState,
    renew_buttonState,
    putIn_key,
    putOut_key,
    )
import qualified Stage          as S
import qualified GameDataCollection     as D
import qualified PlayerState    as P
import qualified QueryPS        as Q
import qualified Configuration  as C

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
main :: IO ()
main =  do
    stage           <- S.createConfigurationStage V.initialGameState
    buttonState     <- I.create_buttonState

    GLUT.initialDisplayMode GLUT.$=  [ GLUT.DoubleBuffered, GLUT.RGBMode ]
    GLUT.initialWindowSize  GLUT.$=  GLUT.Size W.window_sizeX W.window_sizeY

    GLUT.createWindow "puyo"
    
    GLUT.keyboardMouseCallback  GLUT.$= Just (keyboard_callback buttonState)
    GLUT.displayCallback        GLUT.$= display_callback stage
    
    GLUT.addTimerCallback timerInterval $ timer_callback fff buttonState stage
    GLUT.mainLoop
  where
    fff bs stage    = do
        I.renew_buttonState bs
        display_callback stage
        newstage <- explainGame bs stage
        return newstage
    
--------------------------------------------------------------------------------
--  コールバック関数
--------------------------------------------------------------------------------
--  ディスプレイコールバック
display_callback :: S.GameStage -> IO ()
display_callback stage =  do
    GLUT.clear [GLUT.ColorBuffer]
    R.render stage
    GLUT.swapBuffers

--  キーボード・マウスコールバック
keyboard_callback   :: I.ButtonState -> GLUT.Key -> GLUT.KeyState -> t1 -> t2
                    -> IO ()
keyboard_callback stateB k s _ _   
 | k == GLUT.Char 'q'   = EXIT.exitWith EXIT.ExitSuccess
 | s == GLUT.Down       = I.putIn_key  stateB k
 | s == GLUT.Up         = I.putOut_key stateB k
 | otherwise            = return ()

--  タイマコールバック      
timer_callback          :: (I.ButtonState -> S.GameStage -> IO S.GameStage)
                        -> I.ButtonState -> S.GameStage -> IO () 
timer_callback f bs stage =  do
    newstage <- f bs stage
    GLUT.addTimerCallback timerInterval $ timer_callback f bs newstage

timerInterval   =  1000 `quot` W.frame_rate   :: GLUT.Timeout

--------------------------------------------------------------------------------
--  ゲームの更新 
--------------------------------------------------------------------------------
explainGame :: I.ButtonState -> S.GameStage -> IO S.GameStage
explainGame bs stage@(S.Game _ gs (state1P, state2P) gdc)   = do
    flagGameContinue1   <- G.convert_gamePhase state1P bs gs
    flagGameContinue2   <- G.convert_gamePhase state2P bs gs
    initializeGame flagGameContinue1 flagGameContinue2
  where
    initializeGame  :: Bool -> Bool -> IO S.GameStage
    initializeGame False False  = do
        winner  <- Q.get_whoWins state1P
        S.createGameStage gs (D.addWin winner gdc)
    initializeGame _     _      = return stage
explainGame bs stage@(S.Configuration _ _ _)            = do
    C.convertConfigurationPhase bs stage