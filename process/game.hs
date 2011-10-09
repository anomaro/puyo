-- file: gamePhase.hs
module Process.Game
    (
    convert_gamePhase,
    )
    where
import Data.Maybe       (isJust)
import Control.Monad    (when)

import qualified Common.PlayerIdentity  as Identity
import qualified Common.Direction       as Direction
import qualified Common.Time            as Time
import qualified Common.Score           as Score

import qualified Common.DataType   as T
import qualified State.Setting   as V (GameState)

import qualified Input          as I
import qualified Process.Phase.Build    as B
import qualified Process.Phase.Control  as C
import qualified Process.Phase.Drop     as D
import qualified Process.Phase.Erase    as E
import qualified Process.Phase.Fall     as F
import qualified Process.Phase.Gameover as G

import State.Player.DataType
import State.Player.Query   (
    get_playerIdentity,
    get_gamePhase,
    get_PlayerPuyoExistent,
    get_yokoku,
    get_whoWins,
    )
import State.Player.Overwriting (
    shift_gamePhase,
    renewScore,
    )


wrapMaybe       :: Bool -> a -> Maybe a
wrapMaybe p x   =  if p then Just x else Nothing
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
    winPlayer   <- get_whoWins  state
    return $ isJust winPlayer

--------------------------------------------------------------------------------
--  AnimationPhase Time GamePhase   アニメーション硬直
--------------------------------------------------------------------------------
animation        :: Time.Time -> T.GamePhase -> PlayerState -> IO()
animation time nextPhase state
 | time > 0  = shift_gamePhase state 
                $ T.AnimationPhase (Time.count time) nextPhase
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
        case Identity.player $ get_playerIdentity state
          of Identity.User     -> control_playerPuyo' listB            gs state
             Identity.Com name -> control_playerPuyo' I.testButtonList gs state

control_playerPuyo' :: [I.Button] -> V.GameState -> PlayerState -> IO()
control_playerPuyo' listB gs state  = do
    C.fallNatural_playerPuyo state gs
    flag_playerrotate   <- rotate_playerPuyo state listB
    flag_playermove     <- move_playerPuyo gs state listB
    when flag_playermove
     ( get_gamePhase state >>=
       shift_gamePhase state . T.AnimationPhase Time.animeMove )
     
-- ボタン状態を調べてぷよを移動する。
move_playerPuyo :: V.GameState -> PlayerState -> [I.Button] -> IO Bool
move_playerPuyo gs state buttons =
    let r   = wrapMaybe (elem I.right_button buttons) Direction.Right
        l   = wrapMaybe (elem I.left_button  buttons) Direction.Left
        d   = wrapMaybe (elem I.down_button  buttons) Direction.Down
    in  matching d (Direction.compose r l)
  where
    matching Nothing  []    = return False
    matching (Just _) []    = renewScore Score.fallBounus state
                              >> C.fall_puyo state gs
    matching Nothing  (d:_) = C.move_puyo state d
    matching (Just _) (d:_) = C.move_puyo state d >>= \flagMove -> if flagMove
                                  then return flagMove
                                  else renewScore Score.fallBounus state
                                       >> C.fall_puyo state gs

-- ボタン状態を調べてぷよを回転する。
rotate_playerPuyo :: PlayerState -> [I.Button] -> IO Bool
rotate_playerPuyo state buttons =
    let r   = wrapMaybe (elem I.two_button buttons) Direction.Clockwise
        l   = wrapMaybe (elem I.one_button buttons) Direction.CounterClockwise
        coordinates  = Direction.compose r l
    in case coordinates
       of []        -> return False
          (rd:_)    -> C.rotate_puyo state rd >> return True
              
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
                    (T.AnimationPhase Time.animeDrop T.DropPhase)
          else shift_gamePhase state 
                    (T.AnimationPhase Time.animeLand T.ErasePhase)
                    
--------------------------------------------------------------------------------
--  ErasePhase      ぷよの消滅
--------------------------------------------------------------------------------
erase_fieldPuyo             :: V.GameState -> PlayerState -> IO()
erase_fieldPuyo gs state    =  do
    flagErase   <- E.erase_puyo gs state
    if flagErase
      then shift_gamePhase state 
                    $ T.AnimationPhase Time.animeErase T.ErasePhase'
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
    trt = Identity.territory $ get_playerIdentity state
    
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
    loopPhase   = T.AnimationPhase Time.animeDrop T.FallPhase'
    nextPhase   = T.AnimationPhase Time.animeLand T.BuildPhase
    
--------------------------------------------------------------------------------
--  GameoverPhase   ゲームオーバー時処理
--------------------------------------------------------------------------------
gameover            :: V.GameState -> PlayerState -> IO()
gameover gs state   =  do
    return ()
