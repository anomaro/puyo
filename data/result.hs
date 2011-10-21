module Data.Result
( Collection
, wins
, initial
, addWin
) where

import Data.Graph.Inductive.Query.Monad

import qualified Data.PlayerIdentity  as Identity

--------------------------------------------------------------------------------
--  型
--------------------------------------------------------------------------------
data Collection = Collection 
                { wins  :: Wins
                }
--data GameDataCollection = GameData Wins

-- 勝利数 （１プレイヤー, ２プレイヤー）
type Wins       = (NumOfWin, NumOfWin)
type NumOfWin   = Integer

--------------------------------------------------------------------------------
--  初期値
--------------------------------------------------------------------------------
initial =  Collection (0, 0)        :: Collection

--------------------------------------------------------------------------------
--  書き換え
--------------------------------------------------------------------------------
renew                       :: (Wins -> Wins) -> Collection -> Collection
renew fw (Collection ew)    =  Collection (fw ew)
    

-- 敗北していないプレイヤーの勝ち数を１増やす。
addWin  :: Maybe (Identity.Territory) -> Collection -> Collection
addWin Nothing  gdc = gdc
addWin (Just a) gdc = renew (Identity.apply a (+1)) gdc