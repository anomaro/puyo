module State.Player.Substance
    where

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
import Control.Applicative
import qualified Data.IORef         as IORF
import qualified Data.Array.IO      as AIO

import qualified Common.PlayerIdentity  as Identity
import qualified Common.Area            as Area
import qualified Common.Direction       as Direction
import Common.Time  (Time)
import Common.Score (Score)
import Common.Color (Color)
import qualified Common.Field           as Field
import qualified Common.Yokoku          as Yokoku
import qualified Common.Phase           as Phase (Game)

--------------------------------------------------------------------------------
--  プレイヤー状態
--------------------------------------------------------------------------------
-- １人のプレイヤーが保持する状態をひとまとめにしたデータ。
-- （フィールド状態・操作ぷよ・得点など）
data PlayerState = PlayerState  Identity.PlayerIdentity
                                GamePhaseState      -- ゲーム状態遷移
                                FieldState          -- フィールド状態
                                PlayerPuyoState     -- 操作ぷよの状態
                                NextPuyoState       -- ネクストぷよ以降の色
                                ScoreState          -- 得点
                                YokokuState         -- 予告ぷよ（共有）
                                LoseFlagState       -- 敗北フラグ（共有）

type GamePhaseState  = IORF.IORef Phase.Game
type FieldState      = AIO.IOArray Field.Position Area.Area
type PlayerPuyoState = IORF.IORef PlayerPuyo
data PlayerPuyo
    = NonExistent
    | PlayerPuyoInfo    (Color, Color)      -- （基点ぷよの色、動点ぷよの色）
                        Field.Position      -- 基点ぷよのフィールド座標
                        Direction.Area      -- 動点ぷよの方向
                        Time                -- 自然落下用のカウンタ
                        Time                -- 回転用のカウンタ
                        Bool                -- クイックターンフラグ
        deriving (Show, Eq)
        
type NextPuyoState   = IORF.IORef [Color]   
          
type ScoreState      = IORF.IORef Score 

type YokokuState    = IORF.IORef YokokuField
type YokokuField    = (Yokoku.Shelf, Yokoku.Shelf)

type LoseFlagState  = IORF.IORef LoseFlag
type LoseFlag       = (Bool, Bool)

--------------------------------------------------------------------------------
--  状態くり抜き
--------------------------------------------------------------------------------
-- プレイヤー状態から特定の状態をくり抜く。
takeout_playerIdentity  :: PlayerState -> Identity.PlayerIdentity
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