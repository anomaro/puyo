module State.Player.Query
    -- プレーヤー識別子取得。
( get_playerIdentity

    -- ゲーム状態取得
, get_gamePhase
    
    -- フィールドの状態取得
, get_fieldStateArea
, is_neighborSpace
    
    -- 操作ぷよの状態取得
, get_PlayerPuyoExistent
, get_PlayerPuyoColors
, get_PlayerPuyoPosition
, get_PlayerPuyoDirection
, get_PlayerPuyoFallTime
, get_PlayerPuyoRotateTime
, get_PlayerPuyoQuickTurnFlag
    
    -- ネクストぷよの色取得
, get_nextPuyoColors
    
    -- 得点取得
, get_score
    
    -- 予告ぷよ取得
, get_fallOjamaPuyo
, get_yokoku
    
    -- 敗北フラグ
, get_whoWins
    
    -- 状態生成
, create_playerstate
, create_nextPuyoState
, copy_nextPuyoState
, create_yokokuState
, create_loseFlagState
) where

import qualified State.Player.Substance as P'

import Control.Monad
import Control.Applicative
import qualified Data.IORef         as IORF
import qualified Data.Array.IO      as AIO

import qualified Data.Setting          as Setting
import qualified Data.PlayerIdentity  as Identity
import qualified Data.Area            as Area
import qualified Data.Direction       as Direction
import Data.Time  (Time)
import qualified Data.Score           as Score
import Data.Color (Color, determine)
import qualified Data.Field           as Field
import qualified Data.Random          as Random (run, list)
import qualified Data.Yokoku          as Yokoku
import qualified Data.Number          as Number
import qualified Data.Phase           as Phase (Game, start)

--------------------------------------------------------------------------------
(<$<) :: Functor f => (a -> b) -> (c -> f a) -> (c -> f b)
f <$< g =  fmap f . g
infixr 1 <$<

--------------------------------------------------------------------------------
--  プレイヤー状態の内容を外部モジュールに伝える。
--------------------------------------------------------------------------------
-- プレイヤー識別を伝える。
get_playerIdentity  :: P'.PlayerState -> Identity.PlayerIdentity
get_playerIdentity  =  P'.identity

-- ゲーム状態を伝える。
get_gamePhase   :: P'.PlayerState -> IO Phase.Game
get_gamePhase   =  IORF.readIORef . P'.phase

-- ネクストぷよの色を伝える。
get_nextPuyoColors  :: P'.PlayerState -> IO [Color]
get_nextPuyoColors  =  IORF.readIORef . P'.nexts

--------------------------------------------------------------------------------
--  敗北フラグ
--------------------------------------------------------------------------------
-- 勝利したプレイヤーを調べる。
get_whoWins         :: P'.PlayerState -> IO (Maybe Identity.Territory)
get_whoWins state   =  do
    loseFlag    <- IORF.readIORef $ P'.loseFlag state
    return $ check loseFlag
  where
    check (False, True)     = Just Identity.Left
    check (True,  False)    = Just Identity.Right
    check _                 = Nothing

--------------------------------------------------------------------------------
--  予告ぷよ
--------------------------------------------------------------------------------
-- 実際に降るおじゃまぷよの数を伝える。
get_fallOjamaPuyo   :: Identity.Territory -> P'.PlayerState -> IO Number.Puyo
get_fallOjamaPuyo trt state = do
    yokokuField <- IORF.readIORef $ P'.yokoku state
    return $ Yokoku.actual $ (Identity.pick trt) yokokuField

-- 予告ぷよの数を伝える。
get_yokoku  :: Identity.Territory -> P'.PlayerState -> IO Number.Puyo
get_yokoku trt state =  do
    yokokuField <- IORF.readIORef $ P'.yokoku state
    return $ Yokoku.total $ (Identity.pick trt) yokokuField
    
--------------------------------------------------------------------------------
--  得点
--------------------------------------------------------------------------------
-- 得点を伝える。
get_score       :: P'.PlayerState -> IO Score.Score
get_score       =  IORF.readIORef . P'.score

--------------------------------------------------------------------------------
--  フィールドの状態取得
--------------------------------------------------------------------------------
-- 指定したエリアのフィールドのオブジェクトの種類を伝える。
get_fieldStateArea      :: Field.Position -> P'.PlayerState -> IO Area.Area
get_fieldStateArea p    =  (flip AIO.readArray) p . P'.field
                             
-- 指定した方向に隣接するエリアのオブジェクトが、空白かどうか判定。
is_neighborSpace :: Field.Position -> Direction.Area -> P'.PlayerState -> IO Bool
is_neighborSpace p d =
    Area.isSpace <$< (flip AIO.readArray) (Field.neighbor d p) . P'.field

--------------------------------------------------------------------------------
--  操作ぷよの状態取得
--------------------------------------------------------------------------------
readTakePP :: P'.PlayerState -> IO P'.PlayerPuyo
readTakePP =  IORF.readIORef . P'.playerPuyo

-- 操作ぷよが存在するかどうか伝える。
get_PlayerPuyoExistent      :: P'.PlayerState -> IO Bool
get_PlayerPuyoExistent      =  (P'.NonExistent /=) <$< readTakePP

-- 操作ぷよの色の組みを伝える。
get_PlayerPuyoColors        :: P'.PlayerState -> IO (Color, Color)
get_PlayerPuyoColors        =  P'.colors        <$< readTakePP

-- 操作ぷよの基点ぷよのフィールド座標を伝える。
get_PlayerPuyoPosition      :: P'.PlayerState -> IO Field.Position
get_PlayerPuyoPosition      =  P'.position      <$< readTakePP

-- 操作ぷよの動点ぷよの方向を伝える。
get_PlayerPuyoDirection     :: P'.PlayerState -> IO Direction.Area
get_PlayerPuyoDirection     =  P'.direction     <$< readTakePP

-- 操作ぷよの自然落下用のカウンタを伝える。
get_PlayerPuyoFallTime      :: P'.PlayerState -> IO Time
get_PlayerPuyoFallTime      =  P'.fallTime      <$< readTakePP

-- 操作ぷよの回転用のカウンタを伝える。
get_PlayerPuyoRotateTime    :: P'.PlayerState -> IO Time
get_PlayerPuyoRotateTime    =  P'.rotateTime    <$< readTakePP

-- 操作ぷよのクイックターンの可否を伝える。
get_PlayerPuyoQuickTurnFlag :: P'.PlayerState -> IO Bool
get_PlayerPuyoQuickTurnFlag =  P'.quickFlag     <$< readTakePP

--------------------------------------------------------------------------------
--  プレイヤー状態初期化
--------------------------------------------------------------------------------
create_playerstate      :: Identity.PlayerIdentity
                        -> Setting.Setting -> P'.NextPuyoState
                        -> P'.YokokuState -> P'.LoseFlagState
                        -> IO P'.PlayerState
create_playerstate pI gs stateN stateY stateL   =  do
    stateG  <- create_gamePhaseState
    stateF  <- create_fieldstate
    stateP  <- create_ppuyostate
    stateS  <- create_scorestate
    return $ P'.PlayerState pI stateG stateF stateP stateN stateS stateY stateL
  where
    -- 得点の初期状態
    create_scorestate   =  IORF.newIORef Score.initial
    -- ゲーム状態を初期化。
    create_gamePhaseState   :: IO P'.GamePhaseState
    create_gamePhaseState   =  IORF.newIORef Phase.start
    -- 操作ぷよの状態変数を初期化。
    create_ppuyostate   :: IO P'.PlayerPuyoState
    create_ppuyostate   =  IORF.newIORef P'.NonExistent
    -- フィールド状態の可変配列を作成する。
    create_fieldstate :: IO P'.FieldState
    create_fieldstate =  do
        stateF  <- AIO.newArray ((1, 1), (Field.sizeRank gs, Field.sizeLine gs))
                                Area.initialFst
        mapM_ (\p -> AIO.writeArray stateF p Area.initialSnd) spacePositions
        return stateF
      where
        spacePositions = [(r, l)| r <- [1..Field.sizeRank gs - 1]
                                , l <- [2..Field.sizeLine gs - 1]]

-- ネクストぷよを初期化。
create_nextPuyoState        :: Setting.Setting -> IO P'.NextPuyoState
create_nextPuyoState  gs    =  do
    seed    <- Random.run maxBound
    colors  <- Setting.getColorPattern gs
    IORF.newIORef $ map (determine colors)
        $ Random.list (Setting.get Setting.Color gs - 1) seed
        
-- ネクストぷよをコピーする。（２Ｐ側のネクストぷよ状態を作るときに使う。）
copy_nextPuyoState :: P'.NextPuyoState -> IO P'.NextPuyoState
copy_nextPuyoState =  IORF.newIORef <=< IORF.readIORef  

-- 予告ぷよを初期化。
create_yokokuState :: IO P'.YokokuState
create_yokokuState =  IORF.newIORef (Yokoku.initial, Yokoku.initial)

-- 敗北フラグを初期化。
create_loseFlagState    :: IO P'.LoseFlagState
create_loseFlagState    =  IORF.newIORef (False, False)