module Data.PlayerIdentity
( PlayerIdentity(..)
, Territory(..)
, Player(..)
, ComName(..)
, defaultUser1P
, defaultUser2P
, against
, apply
, pick
) where

import Prelude hiding (Left, Right)
import Data.Graph.Inductive.Query.Monad (mapFst, mapSnd)

--------------------------------------------------------------------------------
--  型
--------------------------------------------------------------------------------
-- プレイヤーを識別するデータ。
data PlayerIdentity = PlayerIdentity { territory    :: Territory
                                     , player       :: Player
                                     }

-- 表示フィールドを識別するデータ。
data Territory  = Left
                | Right
        deriving (Show, Eq)
        
-- プレイヤーを表すデータ。
data Player     = User          -- ユーザ
                | Com ComName   -- コンピュータ　名前
data ComName    = Pechipechi

--------------------------------------------------------------------------------
--  名前
--------------------------------------------------------------------------------
-- 標準のユーザ
defaultUser1P   = PlayerIdentity Left  (Com Pechipechi) :: PlayerIdentity
defaultUser2P   = PlayerIdentity Right User             :: PlayerIdentity

--------------------------------------------------------------------------------
--  関数
--------------------------------------------------------------------------------
-- 自分以外の領域
against :: Territory -> Territory
against Left   = Right
against Right  = Left

apply   :: Territory -> (a -> a) -> (a, a) -> (a, a)
apply Left  = mapFst
apply Right = mapSnd

pick    :: Territory -> (a, a) -> a
pick Left   = fst
pick Right  = snd