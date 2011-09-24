-- v_gameDataCollection.hs
-- �������ȂǁA�Q�[�����ʂ̃f�[�^���W�߂Ă܂Ƃ߂Ă����B
module V_GameDataCollection 
  where

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