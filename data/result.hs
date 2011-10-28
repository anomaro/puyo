module Data.Result
( Collection
, wins
, initial
, addWin
) where

import qualified Data.PlayerIdentity  as Identity

--------------------------------------------------------------------------------
--  �^
--------------------------------------------------------------------------------
data Collection = Collection 
                { wins  :: Wins
                }

-- ������ �i�P�v���C���[, �Q�v���C���[�j
type Wins       = (NumOfWin, NumOfWin)
type NumOfWin   = Integer

--------------------------------------------------------------------------------
--  �����l
--------------------------------------------------------------------------------
initial =  Collection (0, 0)        :: Collection

--------------------------------------------------------------------------------
--  ��������
--------------------------------------------------------------------------------
renew                       :: (Wins -> Wins) -> Collection -> Collection
renew fw (Collection ew)    =  Collection (fw ew)
    

-- �s�k���Ă��Ȃ��v���C���[�̏��������P���₷�B
addWin  :: Maybe (Identity.Territory) -> Collection -> Collection
addWin Nothing  gdc = gdc
addWin (Just a) gdc = renew (Identity.apply a (+1)) gdc