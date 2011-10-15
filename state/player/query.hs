module State.Player.Query
    (
    -- プレーヤー識別子取得。
    get_playerIdentity,
    
    -- ゲーム状態取得
    get_gamePhase,
    
    -- フィールドの状態取得
    get_fieldStateArea,
    is_neighborSpace,
    
    -- 操作ぷよの状態取得
    get_PlayerPuyoExistent,
    get_PlayerPuyoColors,
    get_PlayerPuyoPosition,
    get_PlayerPuyoDirection,
    get_PlayerPuyoFallTime,
    get_PlayerPuyoRotateTime,
    get_PlayerPuyoQuickTurnFlag,
    
    -- ネクストぷよの色取得
    get_nextPuyoColors,
    
    -- 得点取得
    get_score,
    
    -- 予告ぷよ取得
    get_fallOjamaPuyo,
    get_yokoku,
    
    -- 敗北フラグ
    get_whoWins,
    
    -- 状態生成
    create_playerstate,
    create_nextPuyoState,
    copy_nextPuyoState,
    create_yokokuState,
    create_loseFlagState,
    )
    where

import qualified State.Player.Substance as P'

import Control.Monad
import Control.Applicative
import qualified Data.IORef         as IORF
import qualified Data.Array.IO      as AIO

import qualified State.Setting          as Setting
import qualified Common.PlayerIdentity  as Identity
import qualified Common.Area            as Area
import qualified Common.Direction       as Direction
import Common.Time  (Time)
import qualified Common.Score           as Score
import Common.Color (Color, determine)
import qualified Common.Field           as Field
import qualified Common.Random          as Random (run, list)
import qualified Common.Yokoku          as Yokoku
import qualified Common.Number          as Number
import qualified Common.Phase           as Phase (Game, start)

--------------------------------------------------------------------------------
(<$<) :: Functor f => (a -> b) -> (c -> f a) -> (c -> f b)
f <$< g =  fmap f . g
infixr 1 <$<

readField   :: (AIO.MArray a e m, AIO.Ix i) => i -> a i e -> m e
readField   =  flip AIO.readArray 

--------------------------------------------------------------------------------
--  プレイヤー状態の内容を外部モジュールに伝える。
--------------------------------------------------------------------------------
-- プレイヤー識別を伝える。
get_playerIdentity  :: P'.PlayerState -> Identity.PlayerIdentity
get_playerIdentity  =  P'.takeout_playerIdentity

-- ゲーム状態を伝える。
get_gamePhase   :: P'.PlayerState -> IO Phase.Game
get_gamePhase   =  IORF.readIORef <=< P'.takeout_gamePhaseState

-- ネクストぷよの色を伝える。
get_nextPuyoColors  :: P'.PlayerState -> IO [Color]
get_nextPuyoColors  =  IORF.readIORef <=< P'.takeout_nextPuyoState

--------------------------------------------------------------------------------
--  敗北フラグ
--------------------------------------------------------------------------------
-- 勝利したプレイヤーを調べる。
get_whoWins         :: P'.PlayerState -> IO (Maybe Identity.Territory)
get_whoWins state   =  do
    loseFlag    <- IORF.readIORef $ P'.takeout_loseFlagState state
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
    yokokuField <- IORF.readIORef $ P'.takeout_yokokuState state
    return $ Yokoku.actual $ (Identity.pick trt) yokokuField

-- 予告ぷよの数を伝える。
get_yokoku  :: Identity.Territory -> P'.PlayerState -> IO Number.Puyo
get_yokoku trt state =  do
    yokokuField <- IORF.readIORef $ P'.takeout_yokokuState state
    return $ Yokoku.total $ (Identity.pick trt) yokokuField
    
--------------------------------------------------------------------------------
--  得点
--------------------------------------------------------------------------------
-- 得点を伝える。
get_score       :: P'.PlayerState -> IO Score.Score
get_score       =  IORF.readIORef . P'.takeout_scoreState

--------------------------------------------------------------------------------
--  フィールドの状態取得
--------------------------------------------------------------------------------
-- 指定したエリアのフィールドのオブジェクトの種類を伝える。
get_fieldStateArea      :: Field.Position -> P'.PlayerState -> IO Area.Area
get_fieldStateArea p    =  readField p <=< P'.takeout_fieldstate
                             
-- 指定した方向に隣接するエリアのオブジェクトが、空白かどうか判定。
is_neighborSpace :: Field.Position -> Direction.Area -> P'.PlayerState -> IO Bool
is_neighborSpace p d =
    Area.isSpace <$< readField (Field.neighbor d p) <=< P'.takeout_fieldstate

--------------------------------------------------------------------------------
--  操作ぷよの状態取得
--------------------------------------------------------------------------------
-- 操作ぷよが存在するかどうか伝える。
get_PlayerPuyoExistent  :: P'.PlayerState -> IO Bool
get_PlayerPuyoExistent  =  (P'.NonExistent /=) <$< readTakePP

-- 操作ぷよの色の組みを伝える。
get_PlayerPuyoColors        :: P'.PlayerState -> IO (Color, Color)
get_PlayerPuyoColors        =  getPPColor           <$< readTakePP

-- 操作ぷよの基点ぷよのフィールド座標を伝える。
get_PlayerPuyoPosition      :: P'.PlayerState -> IO Field.Position
get_PlayerPuyoPosition      =  getPPPosition        <$< readTakePP

-- 操作ぷよの動点ぷよの方向を伝える。
get_PlayerPuyoDirection     :: P'.PlayerState -> IO Direction.Area
get_PlayerPuyoDirection     =  getPPDirection       <$< readTakePP

-- 操作ぷよの自然落下用のカウンタを伝える。
get_PlayerPuyoFallTime      :: P'.PlayerState -> IO Time
get_PlayerPuyoFallTime      =  getPPFallTime        <$< readTakePP

-- 操作ぷよの回転用のカウンタを伝える。
get_PlayerPuyoRotateTime    :: P'.PlayerState -> IO Time
get_PlayerPuyoRotateTime    =  getPPRotateTime      <$< readTakePP

-- 操作ぷよのクイックターンの可否を伝える。
get_PlayerPuyoQuickTurnFlag :: P'.PlayerState -> IO Bool
get_PlayerPuyoQuickTurnFlag =  getPPFlagQuickTurn   <$< readTakePP


readTakePP :: P'.PlayerState -> IO P'.PlayerPuyo
readTakePP =  IORF.readIORef <=< P'.takeout_ppuyostate

getPPColor          (P'.PlayerPuyoInfo c _ _ _ _ _)    = c :: (Color, Color)
getPPPosition       (P'.PlayerPuyoInfo _ p _ _ _ _)    = p :: Field.Position
getPPDirection      (P'.PlayerPuyoInfo _ _ d _ _ _)    = d :: Direction.Area
getPPFallTime       (P'.PlayerPuyoInfo _ _ _ f _ _)    = f :: Time
getPPRotateTime     (P'.PlayerPuyoInfo _ _ _ _ r _)    = r :: Time
getPPFlagQuickTurn  (P'.PlayerPuyoInfo _ _ _ _ _ q)    = q :: Bool


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
    create_fieldstate = 
        AIO.newArray ((1, 1), (Field.sizeRank gs, Field.sizeLine gs)) Area.initialSnd
        >>= \stateF -> build_wall stateF    -- 壁オブジェクトを作る。
        >> return stateF
      where
        build_wall          :: P'.FieldState -> IO()
        build_wall stateF   =  do  
            writeStroke_field stateF walls (fieldArray_indicesY $ Field.sizeRank gs)
            writeStroke_field stateF walls (fieldArray_indicesX $ Field.sizeLine gs)
            writeStroke_field stateF walls (fieldArray_indicesX 1)
          where
            walls = repeat Area.initialFst
            -- フィールドの、リストから得た座標を特定のオブジェクトに書き換える。
            writeStroke_field :: P'.FieldState -> [Area.Area] -> [Field.Position] -> IO()
            writeStroke_field _      _      []      = return ()
            writeStroke_field _      []     _       = return ()
            writeStroke_field stateF (a:as) (p:ps)  =
                AIO.writeArray stateF p a
                >> writeStroke_field stateF as ps
            -- フィールド状態配列の、Y座標を指定した要素のリスト。（X座標は操作ぷよの可視範囲）
            fieldArray_indicesY :: Field.Rank -> [Field.Position]
            fieldArray_indicesY y   
             | 1 <= y && y <= Field.sizeRank gs  = [(y, x) | x <- [2..Field.sizeLine gs - 1] ]
             | otherwise                         = []
            -- フィールド状態配列の、X座標を指定した要素のリスト。
            fieldArray_indicesX :: Field.Rank -> [Field.Position]
            fieldArray_indicesX x   
             | 1 <= x && x <= Field.sizeLine gs  = [(y, x) | y <- [1..Field.sizeRank gs] ]
             | otherwise                         = []


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