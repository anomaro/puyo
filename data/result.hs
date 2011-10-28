module Data.Result
( Collection
, wins
, initial
, addWin
) where

import qualified Data.PlayerIdentity  as Identity

--------------------------------------------------------------------------------
--  Œ^
--------------------------------------------------------------------------------
data Collection = Collection 
                { wins  :: Wins
                }

-- Ÿ—˜” i‚PƒvƒŒƒCƒ„[, ‚QƒvƒŒƒCƒ„[j
type Wins       = (NumOfWin, NumOfWin)
type NumOfWin   = Integer

--------------------------------------------------------------------------------
--  ‰Šú’l
--------------------------------------------------------------------------------
initial =  Collection (0, 0)        :: Collection

--------------------------------------------------------------------------------
--  ‘‚«Š·‚¦
--------------------------------------------------------------------------------
renew                       :: (Wins -> Wins) -> Collection -> Collection
renew fw (Collection ew)    =  Collection (fw ew)
    

-- ”s–k‚µ‚Ä‚¢‚È‚¢ƒvƒŒƒCƒ„[‚ÌŸ‚¿”‚ð‚P‘‚â‚·B
addWin  :: Maybe (Identity.Territory) -> Collection -> Collection
addWin Nothing  gdc = gdc
addWin (Just a) gdc = renew (Identity.apply a (+1)) gdc