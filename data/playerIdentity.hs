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
--  �^
--------------------------------------------------------------------------------
-- �v���C���[�����ʂ���f�[�^�B
data PlayerIdentity = PlayerIdentity { territory    :: Territory
                                     , player       :: Player
                                     }

-- �\���t�B�[���h�����ʂ���f�[�^�B
data Territory  = Left
                | Right
        deriving (Show, Eq)
        
-- �v���C���[��\���f�[�^�B
data Player     = User          -- ���[�U
                | Com ComName   -- �R���s���[�^�@���O
data ComName    = Pechipechi

--------------------------------------------------------------------------------
--  ���O
--------------------------------------------------------------------------------
-- �W���̃��[�U
defaultUser1P   = PlayerIdentity Left  (Com Pechipechi) :: PlayerIdentity
defaultUser2P   = PlayerIdentity Right User             :: PlayerIdentity

--------------------------------------------------------------------------------
--  �֐�
--------------------------------------------------------------------------------
-- �����ȊO�̗̈�
against :: Territory -> Territory
against Left   = Right
against Right  = Left

apply   :: Territory -> (a -> a) -> (a, a) -> (a, a)
apply Left  = mapFst
apply Right = mapSnd

pick    :: Territory -> (a, a) -> a
pick Left   = fst
pick Right  = snd