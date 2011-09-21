module PlayerStateSubstance
    where

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
import Control.Applicative
import qualified Data.IORef         as IORF
import qualified Data.Array.IO      as AIO

import qualified Typedata   as T
import qualified Utility    as U

--------------------------------------------------------------------------------
--  プレイヤー状態
--------------------------------------------------------------------------------
-- １人のプレイヤーが保持する状態をひとまとめにしたデータ。
-- （フィールド状態・操作ぷよ・得点など）
data PlayerState = PlayerState  T.PlayerIdentity
                                GamePhaseState      -- ゲーム状態遷移
                                FieldState          -- フィールド状態
                                PlayerPuyoState     -- 操作ぷよの状態
                                NextPuyoState       -- ネクストぷよ以降の色
                                ScoreState          -- 得点
                                YokokuState         -- 予告ぷよ（共有）
                                LoseFlagState       -- 敗北フラグ（共有）

type GamePhaseState  = IORF.IORef T.GamePhase 
type FieldState      = AIO.IOArray T.AreaPosition T.Area 
type PlayerPuyoState = IORF.IORef PlayerPuyo  
data PlayerPuyo
    = NonExistent
    | PlayerPuyoInfo    (T.Color,T.Color)   -- （基点ぷよの色、動点ぷよの色）
                        T.AreaPosition      -- 基点ぷよのフィールド座標
                        T.Direction         -- 動点ぷよの方向
                        T.Time              -- 自然落下用のカウンタ
                        T.Time              -- 回転用のカウンタ
                        Bool                -- クイックターンフラグ
        deriving (Show, Eq)
        
type NextPuyoState   = IORF.IORef [T.Color]   
          
type ScoreState      = IORF.IORef (T.Score, ScoreCalculation)    
type ScoreCalculation   = ( T.NumOfChain,   -- 連鎖数
                            [T.NumOfUnion], -- 連結数
                            [T.Color]       -- 消したぷよの色
                            )
defaultScoreCalculation = (0, [], [])   :: ScoreCalculation

type YokokuState    = IORF.IORef YokokuField
type YokokuField    = (Yokoku, Yokoku)
type Yokoku     = ( YokokuAdvance T.NumOfPuyo,
                    YokokuSupply  T.NumOfPuyo, 
                    YokokuReserve T.NumOfPuyo )
newtype YokokuAdvance a = Advance a
newtype YokokuSupply  a = Supply  a
newtype YokokuReserve a = Reserve a
-- 予約ぷよを発生させる、ぷよ消去（連鎖）中に一旦Reserveに置かれ、
-- そのぷよ消去が完了したら、ReserveからSupplyに移し、
-- おじゃまぷよが降る側のプレイヤーが着地完了したら、Advanceに置かれる。
-- Advanceに置かれた量のおじゃまぷよが最終的に降ってくる。

instance Functor YokokuAdvance  where
    fmap f (Advance n)  = Advance (f n)
instance Functor YokokuSupply   where
    fmap f (Supply  n)  = Supply  (f n)
instance Functor YokokuReserve  where
    fmap f (Reserve n)  = Reserve (f n)

defaultYokokuField  :: YokokuField
defaultYokokuField  =  ((Advance 0, Supply 0, Reserve 0),
                        (Advance 0, Supply 0, Reserve 0))


type LoseFlagState  = IORF.IORef LoseFlag
type LoseFlag       = (Bool, Bool)

--------------------------------------------------------------------------------
--  状態くり抜き
--------------------------------------------------------------------------------
-- プレイヤー状態から特定の状態をくり抜く。
takeout_playerIdentity  :: PlayerState -> T.PlayerIdentity
takeout_playerIdentity  (PlayerState pI _ _ _ _ _ _ _)  =  pI

takeout_gamePhaseState  :: PlayerState -> IO GamePhaseState
takeout_gamePhaseState  (PlayerState _ gs _ _ _ _ _ _)  =  return gs

takeout_fieldstate      :: PlayerState -> IO FieldState
takeout_fieldstate      (PlayerState _ _ fs _ _ _ _ _)  =  return fs

takeout_ppuyostate      :: PlayerState -> IO PlayerPuyoState
takeout_ppuyostate      (PlayerState _ _ _ ps _ _ _ _)  =  return ps

takeout_nextPuyoState   :: PlayerState -> IO NextPuyoState
takeout_nextPuyoState   (PlayerState _ _ _ _ ns _ _ _)  =  return ns

takeout_scoreState      :: PlayerState -> ScoreState
takeout_scoreState      (PlayerState _ _ _ _ _ sc _ _)  =  sc

takeout_yokokuState     :: PlayerState -> YokokuState
takeout_yokokuState     (PlayerState _ _ _ _ _ _ ys _)  =  ys

takeout_loseFlagState   :: PlayerState -> LoseFlagState
takeout_loseFlagState   (PlayerState _ _ _ _ _ _ _ ls)  =  ls