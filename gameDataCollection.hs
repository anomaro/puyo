-- v_gameDataCollection.hs
-- �������ȂǁA�Q�[�����ʂ̃f�[�^���W�߂Ă܂Ƃ߂Ă����B
module GameDataCollection 
where

import Data.Graph.Inductive.Query.Monad

import qualified Typedata               as T

--------------------------------------------------------------------------------
--  �^
--------------------------------------------------------------------------------
data GameDataCollection = GameData Wins

-- ������ �i�P�v���C���[, �Q�v���C���[�j
type Wins       = (NumOfWin, NumOfWin)
type NumOfWin   = Integer

--------------------------------------------------------------------------------
--  �����l
--------------------------------------------------------------------------------
initialGameDataCollection   :: GameDataCollection
initialGameDataCollection   =  GameData (0, 0)

--------------------------------------------------------------------------------
--  �ǂݎ��
--------------------------------------------------------------------------------
getWins                 :: GameDataCollection -> Wins
getWins (GameData w)    =  w

--------------------------------------------------------------------------------
--  ��������
--------------------------------------------------------------------------------
renewGameDataCollection :: (Wins -> Wins)
                        -> GameDataCollection
                        -> GameDataCollection
renewGameDataCollection fw (GameData ew)    =
    GameData (fw ew)
    

-- �s�k���Ă��Ȃ��v���C���[�̏��������P���₷�B
addWin  :: Maybe (T.Territory) -> GameDataCollection -> GameDataCollection
addWin Nothing  gdc = gdc
addWin (Just a) gdc = renewGameDataCollection (f a (+1)) gdc
  where
    f T.TerritoryLeft   = mapFst
    f T.TerritoryRight  = mapSnd