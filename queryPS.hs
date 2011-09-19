module QueryPS
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
    
    -- 状態生成
    create_playerstate,
    create_nextPuyoState,
    copy_nextPuyoState,
    create_yokokuState,
    )
    where


import qualified PlayerStateSubstance   as P'

import Control.Monad
import Control.Applicative
import qualified Data.IORef         as IORF
import qualified Data.Array.IO      as AIO

import qualified Typedata   as T
import qualified Utility    as U
import qualified Variable   as V


(<$<) :: Functor f => (a -> b) -> (c -> f a) -> (c -> f b)
f <$< g =  fmap f . g
infixr 1 <$<

readField   :: (AIO.MArray a e m, AIO.Ix i) => i -> a i e -> m e
readField   =  flip AIO.readArray 

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--  プレイヤー状態の内容を外部モジュールに伝える。
--------------------------------------------------------------------------------
-- プレイヤー識別を伝える。
get_playerIdentity  :: P'.PlayerState -> T.PlayerIdentity
get_playerIdentity  =  P'.takeout_playerIdentity

-- ゲーム状態を伝える。
get_gamePhase   :: P'.PlayerState -> IO T.GamePhase
get_gamePhase   =  IORF.readIORef <=< P'.takeout_gamePhaseState

-- ネクストぷよの色を伝える。
get_nextPuyoColors  :: P'.PlayerState -> IO [T.Color]
get_nextPuyoColors  =  IORF.readIORef <=< P'.takeout_nextPuyoState

--------------------------------------------------------------------------------
--  予告ぷよ
--------------------------------------------------------------------------------
-- 実際に降るおじゃまぷよの数を伝える。
get_fallOjamaPuyo   :: T.Territory -> P'.PlayerState -> IO T.NumOfPuyo
get_fallOjamaPuyo trt state = do
    yokokuField <- IORF.readIORef $ P'.takeout_yokokuState state
    return $ get $ (f trt) yokokuField
  where
    f T.TerritoryLeft   = fst
    f T.TerritoryRight  = snd
    get (P'.Advance n, _, _)    = n

-- 予告ぷよの数を伝える。
get_yokoku  :: T.Territory -> P'.PlayerState -> IO T.NumOfPuyo
get_yokoku trt state =  do
    yokokuField <- IORF.readIORef $ P'.takeout_yokokuState state
    return $ totalYokoku $ (f trt) yokokuField
  where
    f T.TerritoryLeft   = fst
    f T.TerritoryRight  = snd
    totalYokoku (P'.Advance n1, P'.Supply n2, P'.Reserve n3) = n1 + n2 + n3
    
--------------------------------------------------------------------------------
--  得点
--------------------------------------------------------------------------------
-- 得点を伝える。
get_score       :: P'.PlayerState -> IO T.Score
get_score state =  do
    scores   <- IORF.readIORef $ P'.takeout_scoreState state
    return $ fst scores

--------------------------------------------------------------------------------
--  フィールドの状態取得
--------------------------------------------------------------------------------
-- 指定したエリアのフィールドのオブジェクトの種類を伝える。
get_fieldStateArea      :: T.AreaPosition -> P'.PlayerState -> IO T.Area
get_fieldStateArea p    =  readField p <=< P'.takeout_fieldstate
                             
-- 指定した方向に隣接するエリアのオブジェクトが、空白かどうか判定。
is_neighborSpace :: T.AreaPosition -> T.Direction -> P'.PlayerState -> IO Bool
is_neighborSpace p d =
    (T.Space ==) <$< readField (U.neighbor_area d p) <=< P'.takeout_fieldstate

--------------------------------------------------------------------------------
--  操作ぷよの状態取得
--------------------------------------------------------------------------------
-- 操作ぷよが存在するかどうか伝える。
get_PlayerPuyoExistent  :: P'.PlayerState -> IO Bool
get_PlayerPuyoExistent  =  (P'.NonExistent /=) <$< readTakePP

-- 操作ぷよの色の組みを伝える。
get_PlayerPuyoColors        :: P'.PlayerState -> IO (T.Color,T.Color)
get_PlayerPuyoColors        =  getPPColor           <$< readTakePP

-- 操作ぷよの基点ぷよのフィールド座標を伝える。
get_PlayerPuyoPosition      :: P'.PlayerState -> IO T.AreaPosition
get_PlayerPuyoPosition      =  getPPPosition        <$< readTakePP

-- 操作ぷよの動点ぷよの方向を伝える。
get_PlayerPuyoDirection     :: P'.PlayerState -> IO T.Direction
get_PlayerPuyoDirection     =  getPPDirection       <$< readTakePP

-- 操作ぷよの自然落下用のカウンタを伝える。
get_PlayerPuyoFallTime      :: P'.PlayerState -> IO T.Time
get_PlayerPuyoFallTime      =  getPPFallTime        <$< readTakePP

-- 操作ぷよの回転用のカウンタを伝える。
get_PlayerPuyoRotateTime    :: P'.PlayerState -> IO T.Time
get_PlayerPuyoRotateTime    =  getPPRotateTime      <$< readTakePP

-- 操作ぷよのクイックターンの可否を伝える。
get_PlayerPuyoQuickTurnFlag :: P'.PlayerState -> IO Bool
get_PlayerPuyoQuickTurnFlag =  getPPFlagQuickTurn   <$< readTakePP


readTakePP :: P'.PlayerState -> IO P'.PlayerPuyo
readTakePP =  IORF.readIORef <=< P'.takeout_ppuyostate

getPPColor          (P'.PlayerPuyoInfo c _ _ _ _ _)    = c :: (T.Color,T.Color)
getPPPosition       (P'.PlayerPuyoInfo _ p _ _ _ _)    = p :: T.AreaPosition
getPPDirection      (P'.PlayerPuyoInfo _ _ d _ _ _)    = d :: T.Direction
getPPFallTime       (P'.PlayerPuyoInfo _ _ _ f _ _)    = f :: T.Time
getPPRotateTime     (P'.PlayerPuyoInfo _ _ _ _ r _)    = r :: T.Time
getPPFlagQuickTurn  (P'.PlayerPuyoInfo _ _ _ _ _ q)    = q :: Bool


--------------------------------------------------------------------------------
--  プレイヤー状態初期化
--------------------------------------------------------------------------------
create_playerstate      :: T.PlayerIdentity -> V.GameState -> P'.NextPuyoState
                        -> P'.YokokuState -> IO P'.PlayerState
create_playerstate pI gs stateN stateY    =  do
    stateG  <- create_gamePhaseState
    stateF  <- create_fieldstate
    stateP  <- create_ppuyostate
    stateS  <- create_scorestate
    return $ P'.PlayerState pI stateG stateF stateP stateN stateS stateY
  where
    -- 得点の初期状態
    create_scorestate   =  IORF.newIORef initalScore
    initalScore = ( (T.Score 0 0), P'.defaultScoreCalculation )
    -- ゲーム状態を初期化。
    create_gamePhaseState   :: IO P'.GamePhaseState
    create_gamePhaseState   =  IORF.newIORef T.BuildPhase 
    -- 操作ぷよの状態変数を初期化。
    create_ppuyostate   :: IO P'.PlayerPuyoState
    create_ppuyostate   =  IORF.newIORef P'.NonExistent
    -- フィールド状態の可変配列を作成する。
    create_fieldstate :: IO P'.FieldState
    create_fieldstate = 
        AIO.newArray ((1, 1), (V.fieldSizeY' gs, V.fieldSizeX' gs)) T.Space
        >>= \stateF -> build_wall stateF    -- 壁オブジェクトを作る。
        >> return stateF
      where
        build_wall          :: P'.FieldState -> IO()
        build_wall stateF   =  do  
            writeStroke_field stateF walls (fieldArray_indicesY $ V.fieldSizeY' gs)
            writeStroke_field stateF walls (fieldArray_indicesX $ V.fieldSizeX' gs)
            writeStroke_field stateF walls (fieldArray_indicesX 1)
          where
            walls = repeat T.Wall
            -- フィールドの、リストから得た座標を特定のオブジェクトに書き換える。
            writeStroke_field :: P'.FieldState -> [T.Area] -> [T.AreaPosition] -> IO()
            writeStroke_field _      _      []      = return ()
            writeStroke_field _      []     _       = return ()
            writeStroke_field stateF (a:as) (p:ps)  =
                AIO.writeArray stateF p a
                >> writeStroke_field stateF as ps
            -- フィールド状態配列の、Y座標を指定した要素のリスト。（X座標は操作ぷよの可視範囲）
            fieldArray_indicesY :: T.PositionY -> [T.AreaPosition]
            fieldArray_indicesY y   
             | 1 <= y && y <= V.fieldSizeY' gs  = [(y, x) | x <- [2..V.fieldSizeX' gs - 1] ]
             | otherwise                        = []
            -- フィールド状態配列の、X座標を指定した要素のリスト。
            fieldArray_indicesX :: T.PositionX -> [T.AreaPosition]
            fieldArray_indicesX x   
             | 1 <= x && x <= V.fieldSizeX' gs  = [(y, x) | y <- [1..V.fieldSizeY' gs] ]
             | otherwise                        = []


---- ネクストぷよを初期化。
create_nextPuyoState        :: V.GameState -> IO P'.NextPuyoState
create_nextPuyoState  gs    =  do
    seed    <- U.runRandom maxBound
    colors  <- V.get_ColorPattern gs
    IORF.newIORef 
        $ map (V.makeColor colors) $ U.makeRandoms (V.get V.Color gs - 1) seed
        
---- ネクストぷよをコピーする。（２Ｐ側のネクストぷよ状態を作るときに使う。）
copy_nextPuyoState :: P'.NextPuyoState -> IO P'.NextPuyoState
copy_nextPuyoState =  IORF.newIORef <=< IORF.readIORef  


---- 予告ぷよを初期化。
create_yokokuState :: IO P'.YokokuState
create_yokokuState =  IORF.newIORef P'.defaultYokokuField