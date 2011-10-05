module Standardizable
where

class Standardizable a where
    standard        :: a


orStandardIf :: (Standardizable a) => a -> Bool -> a
_   `orStandardIf` False    = standard
opt `orStandardIf` True     = opt