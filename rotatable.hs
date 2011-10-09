{-# LANGUAGE BangPatterns #-}
{-  
    This module implements algebraic data types rotatable.
    For instance, a data that represents seasons can rotate,
    such as Spring => Summer => Autumn => Winter => Spring => ...
-}
module Rotatable
( Rotatable
, skip, backskip
, walk, backwalk
, opposite

, rotationDefaultFast
, rotationDefaultSafe
) where

--------------------------------------------------------------------------------
--  Class
--------------------------------------------------------------------------------
class Rotatable a where
    skip                :: Int -> a -> a
    walk, backwalk      :: a -> a
    
    skip  n | 0 <= n    =  apply n          walk
            | otherwise =  apply (negate n) backwalk
    walk                =  skip     1
    backwalk            =  backskip 1

infix 5 `skip`, `backskip`
backskip    :: (Rotatable a) => Int -> a -> a
backskip    =  skip . negate

opposite                :: (Enum a, Bounded a, Rotatable a) => a -> Maybe a
opposite x  | even e    =  Just $ e `div` 2 `skip` x
            | otherwise =  Nothing
  where e  = size x

--------------------------------------------------------------------------------
--  Implementation
--------------------------------------------------------------------------------
--  fast
rotationDefaultFast      :: (Enum a, Bounded a) => Int -> a -> a
rotationDefaultFast n x  =  toEnum $ (n + fromEnum x) `mod` (size x)

-- safe
rotationDefaultSafe     :: (Eq a, Enum a, Bounded a) => Int -> a -> a
rotationDefaultSafe !n !x
    | 0 == n            =  x
    | 0 < n             =  rotationDefaultSafe (n-1) (forwardDefault  x)
    | otherwise         =  rotationDefaultSafe (n+1) (backwardDefault x)
 where
    forwardDefault  :: (Eq a, Enum a, Bounded a) => a -> a
    forwardDefault  x   | x == maxBound     =  minBound
                        | otherwise         =  succ x
    backwardDefault :: (Eq a, Enum a, Bounded a) => a -> a
    backwardDefault x   | x == minBound     =  maxBound
                        | otherwise         =  pred x


--------------------------------------------------------------------------------
--  inner functions
--------------------------------------------------------------------------------
size    :: (Enum a, Bounded a) => a -> Int
size x  =  1 + (fromEnum $ maxData x)
  where
    maxData     :: (Bounded a) => a -> a
    maxData _   =  maxBound

apply                       :: Int -> (a -> a) -> a -> a
apply !n !f !x  | 0 >= n    =  x
                | otherwise =  apply (n - 1) f (f x)