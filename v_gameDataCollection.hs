-- v_gameDataCollection.hs
-- 勝利数など、ゲーム結果のデータを集めてまとめておく。
module V_GameDataCollection 
  where

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