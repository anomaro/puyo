module State.Player.Overwriting
    (   
    -- ゲーム状態遷移更新
    shift_gamePhase,
    
    -- ネクストぷよ状態更新
    eat_nextPuyo,
    
    -- フィールド状態更新
    renew_fieldArea,
    renew_animationType,
    
    -- 配ぷよ状態更新
    renew_playerPuyo,
    renew_playerPuyo',
    remove_playerPuyo,
    
    -- 得点更新
    renewScoreCalculation,
    renewScore,
    calculateScoreD,
    
    -- 予告ぷよ更新
    renewYokoku,
    toSupplyYokoku,
    toAdvanceYokoku,
    
    -- 敗北フラグ更新
    renewLoseFlag,
    )
    where

import State.Player.Substance
import State.Player.Query

import Data.List    (nub)
import Data.Maybe   (fromMaybe)
import Control.Monad
import Control.Applicative
import qualified Data.IORef         as IORF
import qualified Data.Array.IO      as AIO

import qualified Common.DataType   as T
import qualified Common.Function    as U
import qualified State.Setting  as V

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--  状態書き換え
--------------------------------------------------------------------------------
-- ゲーム状態を変える。
shift_gamePhase                 :: PlayerState -> T.GamePhase -> IO()
shift_gamePhase state gamephase =
    flip IORF.writeIORef gamephase =<< takeout_gamePhaseState state

-- ネクストぷよを消費する。
eat_nextPuyo    :: PlayerState -> T.NumOfPuyo -> IO()
eat_nextPuyo state n =
    flip IORF.modifyIORef (drop n) =<< takeout_nextPuyoState state
    
--------------------------------------------------------------------------------
--  敗北フラグ更新
--------------------------------------------------------------------------------
-- 敗北フラグを更新する。
renewLoseFlag               :: Bool -> T.Territory -> PlayerState -> IO()
renewLoseFlag b trt state   =  do
    loseFlag    <- IORF.readIORef refLoseFlag
    IORF.writeIORef refLoseFlag $ applyTRT trt (const b) loseFlag
  where
    applyTRT T.TerritoryLeft  f (l, r)  = (f l, r  )
    applyTRT T.TerritoryRight f (l, r)  = (l  , f r)
    refLoseFlag = takeout_loseFlagState state

--------------------------------------------------------------------------------
--  予告ぷよ更新
--------------------------------------------------------------------------------
-- 予告ぷよを更新する。
renewYokoku :: (T.NumOfPuyo -> T.NumOfPuyo)
            -> (T.NumOfPuyo -> T.NumOfPuyo)
            -> (T.NumOfPuyo -> T.NumOfPuyo)
            -> T.Territory -> PlayerState -> IO()
renewYokoku f1 f2 f3 trt state   = do
    yokokuField <- IORF.readIORef $ takeout_yokokuState state
    IORF.writeIORef (takeout_yokokuState state) $ applyTRT trt yokokuField
  where
    applyTRT T.TerritoryLeft  (a, b) = (apply a, b      )
    applyTRT T.TerritoryRight (a, b) = (a      , apply b)
    apply (n1, n2, n3) = (fmap f1 n1, fmap f2 n2, fmap f3 n3)

-- 予告ぷよをReserveからSupplyへ移す。
toSupplyYokoku :: T.Territory -> PlayerState -> IO()
toSupplyYokoku trt state =  do
    yokokuField <- IORF.readIORef stateY
    IORF.writeIORef stateY $ applyTRT trt shift yokokuField
  where
    stateY  = takeout_yokokuState state
    applyTRT T.TerritoryLeft  f (l, r)  = (f l, r  )
    applyTRT T.TerritoryRight f (l, r)  = (l  , f r)
    shift (n, Supply n2, Reserve n3)    = (n, Supply $ n2 + n3, Reserve 0)

-- 予告ぷよをSupplyからAdvanceへ移す。
toAdvanceYokoku :: T.NumOfPuyo -> T.Territory -> PlayerState -> IO()
toAdvanceYokoku m trt state =  do
    yokokuField <- IORF.readIORef stateY
    IORF.writeIORef stateY $ applyTRT trt shift yokokuField
  where
    stateY  = takeout_yokokuState state
    applyTRT T.TerritoryLeft  f (l, r)  = (f l, r  )
    applyTRT T.TerritoryRight f (l, r)  = (l  , f r)
    shift (Advance n1, Supply n2, n)
        | n2 > m    = (Advance $ n1 + m , Supply $ n2 - m, n)
        | otherwise = (Advance $ n1 + n2, Supply 0       , n)

--------------------------------------------------------------------------------
--  得点更新
--------------------------------------------------------------------------------
-- 得点のScoreCalculationを書き換える。
renewScoreCalculation :: (T.NumOfChain -> T.NumOfChain) 
                      -> ([T.NumOfUnion] -> [T.NumOfUnion]) 
                      -> ([T.Color] -> [T.Color]) 
                      -> PlayerState -> IO()
renewScoreCalculation f f' f'' state =  do
    (score, (chain, ns, cs) ) <- IORF.readIORef $ takeout_scoreState state
    IORF.writeIORef (takeout_scoreState state)
                    ( score, (f chain, f' ns, f'' cs) )

-- 得点を書き換える。
renewScore  :: (T.StaticScore -> T.StaticScore)
            -> (T.DynamicScore -> T.DynamicScore)
            -> PlayerState -> IO()
renewScore f f' state   =  do
    (T.Score ss sd, sc )    <- IORF.readIORef $ takeout_scoreState state
    IORF.writeIORef (takeout_scoreState state) ( T.Score (f ss) (f' sd), sc )

-- 得点のScoreCalculationを計算して、DynamicScoreを書き換える。
calculateScoreD :: PlayerState -> IO()
calculateScoreD state   =  do
    (score, (chain, ns, cs) ) <- IORF.readIORef $ takeout_scoreState state
    let basicbounus = calculateBasicBounus  
                        $ calculateBounusChain chain
                        + (calculateBounusColor $ length $ nub $ cs)
                        + (sum $ map calculateBounusLink ns)
        newScore    = score .+ 10 * basicbounus * sum ns
    IORF.writeIORef (takeout_scoreState state)
                    (newScore, (chain, [], []))

-- 基本ボーナス値を算出する。
calculateBasicBounus                :: Int -> Int
calculateBasicBounus n  | n <= 0    =  1
                        | n >  1000 =  999
                        | otherwise =  n
-- 連鎖ボーナスを算出する。
calculateBounusChain                :: Int -> Int
calculateBounusChain n  | n < 4     =  8 * truncate ((^^) 2 $ n - 2)
                        | otherwise =  (n - 3) * 32
                        
-- 連結ボーナスを算出する。
calculateBounusLink                 :: Int -> Int
calculateBounusLink n   | n <= 4    =  0
                        | n >= 11   =  10
                        | otherwise =  n - 3
-- 複色ボーナスを算出する。
calculateBounusColor                :: Int -> Int
calculateBounusColor                =  (*) 3 . truncate . (^^) 2 .(+) (-2)

-- おじゃまぷよに換算されていないスコアに足す。
(.+) :: T.Score -> T.ScoreBaseType -> T.Score
(T.Score ss sd) .+ n    = T.Score ss $ sd + n
infixl 5 .+

--  1 	2 	3 	4 	5 	6 	7 	8 	9 	10 	11 	12 	13 	14 	15 	16 	17 	18 	19
--  0 	8 	16 	32 	64 	128 256 512 999 999 999 999 999 999 999 999 999 999 999
--  0 	8 	16 	32 	64 	96 	128 160 192 224 256 288 320 352 384 416 448 480 512

--------------------------------------------------------------------------------
--  フィールド状態更新
--------------------------------------------------------------------------------
-- エリアを更新する。
renew_fieldArea :: PlayerState -> T.AreaPosition -> T.Area -> IO()
renew_fieldArea state p area    =
    takeout_fieldstate state >>= \stateF -> AIO.writeArray stateF p area

-- アニメーション状態を更新する。
renew_animationType :: PlayerState -> T.AreaPosition -> IO()
renew_animationType state p =  do
    stateF  <- takeout_fieldstate state
    area    <- get_fieldStateArea p state
    AIO.writeArray stateF p $ countArea area

countArea :: T.Area -> T.Area
countArea (T.Puyo c uc at)  = T.Puyo c uc $ countAT at
countArea (T.Ojama  uc at)  = T.Ojama  uc $ countAT at
countArea a                 = a

countAT :: T.AnimationType -> T.AnimationType
countAT ( T.Dropping n   )  | n > 0 = T.Dropping $ n - 1
countAT ( T.Landing  n p )  | n > 0 = T.Landing  ( n - 1 ) p
countAT ( T.Erasing  n   )  | n > 0 = T.Erasing  $ n - 1
countAT _                           = T.Normal

--------------------------------------------------------------------------------
--  配ぷよ状態更新
--------------------------------------------------------------------------------
-- 部分書き換え
renew_playerPuyo :: PlayerState 
                    -> Maybe (T.Color, T.Color) -- （基点ぷよの色、動点ぷよの色）
                    -> Maybe T.AreaPosition     -- 基点ぷよのフィールド座標
                    -> Maybe T.Direction        -- 動点ぷよの方向
                    -> Maybe T.Time             -- 自然落下用のカウンタ
                    -> Maybe T.Time             -- 回転用のカウンタ
                    -> Maybe Bool               -- クイックターンフラグ
                    -> IO ()
renew_playerPuyo state c p d tf tt qf   = do
    stateP                              <- takeout_ppuyostate state
    PlayerPuyoInfo c' p' d' tf' tt' qf' <- IORF.readIORef stateP
    IORF.writeIORef stateP $ PlayerPuyoInfo (fromMaybe c'  c )
                                            (fromMaybe p'  p )
                                            (fromMaybe d'  d )
                                            (fromMaybe tf' tf)
                                            (fromMaybe tt' tt)
                                            (fromMaybe qf' qf)

-- 完全書き換え
renew_playerPuyo'   :: PlayerState 
                    -> (T.Color, T.Color) -- （基点ぷよの色、動点ぷよの色）
                    -> T.AreaPosition     -- 基点ぷよのフィールド座標
                    -> T.Direction        -- 動点ぷよの方向
                    -> T.Time             -- 自然落下用のカウンタ
                    -> T.Time             -- 回転用のカウンタ
                    -> Bool               -- クイックターンフラグ
                    -> IO ()
renew_playerPuyo' state c p d tf tt qf  =
    flip IORF.writeIORef (PlayerPuyoInfo c p d tf tt qf) 
                                        =<< takeout_ppuyostate state

-- 完全書き換え
remove_playerPuyo :: PlayerState -> IO()
remove_playerPuyo =  flip IORF.writeIORef NonExistent <=< takeout_ppuyostate