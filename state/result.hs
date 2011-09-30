-- v_gameDataCollection.hs
-- 勝利数など、ゲーム結果のデータを集めてまとめておく。
module State.Result
where

import Data.Graph.Inductive.Query.Monad

import qualified Common.DataType       as T
import qualified Common.PlayerIdentity  as Identity

--------------------------------------------------------------------------------
--  型
--------------------------------------------------------------------------------
data GameDataCollection = GameData Wins

-- 勝利数 （１プレイヤー, ２プレイヤー）
type Wins       = (NumOfWin, NumOfWin)
type NumOfWin   = Integer

--------------------------------------------------------------------------------
--  初期値
--------------------------------------------------------------------------------
initialGameDataCollection   :: GameDataCollection
initialGameDataCollection   =  GameData (0, 0)

--------------------------------------------------------------------------------
--  読み取り
--------------------------------------------------------------------------------
getWins                 :: GameDataCollection -> Wins
getWins (GameData w)    =  w

--------------------------------------------------------------------------------
--  書き換え
--------------------------------------------------------------------------------
renewGameDataCollection :: (Wins -> Wins)
                        -> GameDataCollection
                        -> GameDataCollection
renewGameDataCollection fw (GameData ew)    =
    GameData (fw ew)
    

-- 敗北していないプレイヤーの勝ち数を１増やす。
addWin  :: Maybe (Identity.Territory) -> GameDataCollection -> GameDataCollection
addWin Nothing  gdc = gdc
addWin (Just a) gdc = renewGameDataCollection (Identity.apply a (+1)) gdc