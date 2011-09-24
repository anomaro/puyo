-- file: gamePhase.hs
module GamePhase
    (
    convert_gamePhase,
    )
    where

import qualified Typedata   as T
import qualified Utility    as U ((<-+->))
import qualified Variable   as V (GameState)
import qualified World      as W (amimeTime_move, amimeTime_drop,
                                  amimeTime_land, amimeTime_erase)
import qualified Input          as I
import qualified BuildPhase     as B
import qualified ControlPhase   as C
import qualified DropPhase      as D
import qualified ErasePhase     as E
import qualified FallPhase      as F
import qualified GameoverPhase  as G

import PlayerState
import QueryPS (
    get_playerIdentity,
    get_gamePhase,
    get_PlayerPuyoExistent,
    get_yokoku,
    get_loseFlag,
    )
import OverwritingPS   (
    shift_gamePhase,
    renewScore,
    )

--------------------------------------------------------------------------------
--  ゲーム状態遷移
--------------------------------------------------------------------------------
-- ゲームオーバーフェーズに移行していた場合、Falseを返す。
convert_gamePhase :: PlayerState -> I.ButtonState -> V.GameState -> IO Bool
convert_gamePhase state stateB gs =
    get_gamePhase state 
    >>= \gamePhase  -> (convert_gamePhase' gamePhase) state
    >>  return (gameContinue gamePhase)
  where
    convert_gamePhase' :: T.GamePhase -> PlayerState -> IO ()
    convert_gamePhase' T.BuildPhase             = build_playerPuyo  gs
    convert_gamePhase' T.ControlPhase           = control_playerPuyo stateB gs
    convert_gamePhase' T.DropPhase              = drop_fieldPuyo    gs
    convert_gamePhase' T.ErasePhase             = erase_fieldPuyo   gs
    convert_gamePhase' T.ErasePhase'            = erase_fieldPuyo'  gs
    convert_gamePhase' T.FallPhase              = fall_ojamaPuyo    gs
    convert_gamePhase' T.FallPhase'             = fall_ojamaPuyo'   gs
    convert_gamePhase' T.GameOverPhase          = gameover          gs
    convert_gamePhase' (T.AnimationPhase t g)   = animation t g
    
    gameContinue    :: T.GamePhase -> Bool
    gameContinue T.GameOverPhase    = False
    gameContinue _                  = True

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--  ゲーム終了判定 （ゲーム終了ならTrue）
--------------------------------------------------------------------------------
check_gameEnd       :: PlayerState -> IO Bool
check_gameEnd state =  do
    flagL   <- get_loseFlag T.TerritoryLeft  state
    flagR   <- get_loseFlag T.TerritoryRight state
    return $ flagL || flagR

--------------------------------------------------------------------------------
--  AnimationPhase Time GamePhase   アニメーション硬直
--------------------------------------------------------------------------------
animation        :: T.Time -> T.GamePhase -> PlayerState -> IO()
animation time nextPhase state
 | time > 0  = shift_gamePhase state $ T.AnimationPhase (time - 1) nextPhase
 | otherwise = shift_gamePhase state nextPhase

--------------------------------------------------------------------------------
--  BuildPhase      ぷよ生成
--------------------------------------------------------------------------------
build_playerPuyo            :: V.GameState -> PlayerState -> IO()
build_playerPuyo gs state   =  do
    loseFlag    <- B.checkLose gs state
    if loseFlag
      then shift_gamePhase state T.GameOverPhase
      else B.build_playerPuyo gs state >> shift_gamePhase state T.ControlPhase
    
--------------------------------------------------------------------------------
--  ContralPhase    操作ぷよの更新
--------------------------------------------------------------------------------
control_playerPuyo :: I.ButtonState -> V.GameState -> PlayerState -> IO()
control_playerPuyo stateB gs state = do
    flagGameEnd <- check_gameEnd state
    if flagGameEnd
      then shift_gamePhase state T.GameOverPhase
      else do
        listB <- I.read_buttonState stateB
        case snd $ get_playerIdentity state
          of T.User     -> control_playerPuyo' listB            gs state
             T.Com name -> control_playerPuyo' I.testButtonList gs state

control_playerPuyo' :: [I.Button] -> V.GameState -> PlayerState -> IO()
control_playerPuyo' listB gs state = do
    flag_playermove     <- move_playerPuyo gs state listB       -- ぷよ移動
    flag_playerrotate   <- rotate_playerPuyo state listB        -- ぷよ回転
    flag_fallNatural    <- C.fallNatural_playerPuyo state gs    -- 自然落下
    if flag_playermove -- || flag_playerrotate
     then do nextPhase <- get_gamePhase state
             shift_gamePhase state 
                (T.AnimationPhase W.amimeTime_move nextPhase)
     else return ()
     
-- ボタン状態を調べてぷよを移動する。
move_playerPuyo :: V.GameState -> PlayerState -> [I.Button] -> IO Bool
move_playerPuyo gs state buttons =
    let r   = if elem I.right_button buttons then T.DRight else T.DPoint
        l   = if elem I.left_button  buttons then T.DLeft  else T.DPoint
        d   = if elem I.down_button  buttons then T.DDown  else T.DPoint
        coordinates  = (d ,r  U.<-+->  l)
    in case coordinates
       of (T.DPoint, T.DPoint)  -> return False
          (T.DDown , T.DPoint)  -> renewScore id (1+) state >>
                                   C.fall_puyo state gs >>= return
          (T.DPoint, d)         -> C.move_puyo state d  >>= return  
          (T.DDown , d)         -> do flagMove <- C.move_puyo state d
                                      if flagMove
                                        then return flagMove
                                        else renewScore id (1+) state >>
                                             C.fall_puyo state gs >>= return
-- ボタン状態を調べてぷよを回転する。
rotate_playerPuyo :: PlayerState -> [I.Button] -> IO Bool
rotate_playerPuyo state buttons =
    let r   = if elem I.two_button buttons  then T.RRight else T.RPoint
        l   = if elem I.one_button buttons  then T.RLeft  else T.RPoint
        coordinates  = r  U.<-+->  l
    in case coordinates
       of T.RPoint  -> return False
          rd        -> C.rotate_puyo state rd >> return True
              
--------------------------------------------------------------------------------
--  DropPhase       ちぎりやぷよ消滅後に起こるぷよ落下
--------------------------------------------------------------------------------
drop_fieldPuyo          :: V.GameState -> PlayerState -> IO()
drop_fieldPuyo gs state =  do
    flagExistent <- get_PlayerPuyoExistent state
    if flagExistent
      then do
        D.land_puyo state
        shift_gamePhase state T.DropPhase
      else do
        flagDrop <- D.drop_puyo gs state
        if flagDrop
          then shift_gamePhase state 
                    (T.AnimationPhase W.amimeTime_drop T.DropPhase)
          else shift_gamePhase state 
                    (T.AnimationPhase W.amimeTime_land T.ErasePhase)
                    
--------------------------------------------------------------------------------
--  ErasePhase      ぷよの消滅
--------------------------------------------------------------------------------
erase_fieldPuyo             :: V.GameState -> PlayerState -> IO()
erase_fieldPuyo gs state    =  do
    flagErase   <- E.erase_puyo gs state
    if flagErase
      then shift_gamePhase state 
                    $ T.AnimationPhase W.amimeTime_erase T.ErasePhase'
      else shift_gamePhase state T.FallPhase


erase_fieldPuyo'            :: V.GameState -> PlayerState -> IO()
erase_fieldPuyo' gs state   =  E.rewriteSpase_puyo gs state
                               >> shift_gamePhase state T.DropPhase

--------------------------------------------------------------------------------
--  FallPhase       おじゃまぷよの落下
--------------------------------------------------------------------------------
fall_ojamaPuyo  :: V.GameState -> PlayerState -> IO()
fall_ojamaPuyo gs state =  do
    F.moveYokokuPuyo gs state
    ojama   <- get_yokoku trt state
    if ojama > 0
      then shift_gamePhase state T.FallPhase'
      else shift_gamePhase state T.BuildPhase
  where
    trt = fst $ get_playerIdentity state
    

fall_ojamaPuyo' :: V.GameState -> PlayerState -> IO()
fall_ojamaPuyo' gs state =  do
    flagOjama   <- F.putOjamaPuyo gs state
    if flagOjama
      then do
        flagDrop <- D.drop_puyo gs state
        shift_gamePhase state loopPhase
      else do
        flagDrop <- D.drop_puyo gs state
        if flagDrop
          then shift_gamePhase state loopPhase
          else shift_gamePhase state nextPhase
  where
    loopPhase   = T.AnimationPhase W.amimeTime_drop T.FallPhase'
    nextPhase   = T.AnimationPhase W.amimeTime_land T.BuildPhase
    
--------------------------------------------------------------------------------
--  GameoverPhase   ゲームオーバー時処理
--------------------------------------------------------------------------------
gameover            :: V.GameState -> PlayerState -> IO()
gameover gs state   =  do
    return ()
