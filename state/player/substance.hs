module State.Player.Substance
where

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
import Control.Applicative
import qualified Data.IORef         as IORF
import qualified Data.Array.IO      as AIO

import Data.PlayerIdentity (PlayerIdentity)
import Data.Area (Area)
import qualified Data.Direction       as Direction (Area)
import Data.Time  (Time)
import Data.Score (Score)
import Data.Color (Color)
import qualified Data.Field           as Field (Position)
import qualified Data.Yokoku          as Yokoku (Shelf)
import qualified Data.Phase           as Phase (Game)

--------------------------------------------------------------------------------
--  プレイヤー状態
--------------------------------------------------------------------------------
-- １人のプレイヤーが保持する状態をひとまとめにしたデータ。
-- （フィールド状態・操作ぷよ・得点など）
data PlayerState = PlayerState
                    { identity      :: PlayerIdentity
                    , phase         :: GamePhaseState
                    , field         :: FieldState
                    , playerPuyo    :: PlayerPuyoState
                    , nexts         :: NextPuyoState
                    , score         :: ScoreState
                    , yokoku        :: YokokuState
                    , loseFlag      :: LoseFlagState
                    }

type GamePhaseState  = IORF.IORef Phase.Game
type FieldState      = AIO.IOArray Field.Position Area
type PlayerPuyoState = IORF.IORef PlayerPuyo
data PlayerPuyo
    = NonExistent
    | PlayerPuyoInfo    { colors        :: (Color, Color)
                        , position      :: Field.Position
                        , direction     :: Direction.Area
                        , fallTime      :: Time
                        , rotateTime    :: Time
                        , quickFlag     :: Bool
                        } deriving (Show, Eq)

type NextPuyoState   = IORF.IORef [Color]   
          
type ScoreState      = IORF.IORef Score 

type YokokuState    = IORF.IORef YokokuField
type YokokuField    = (Yokoku.Shelf, Yokoku.Shelf)

type LoseFlagState  = IORF.IORef LoseFlag
type LoseFlag       = (Bool, Bool)