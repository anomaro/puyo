-- variable.hs
module State.Setting
    (
    GameState,
    get,
    get_ColorPattern,
    newGameState,

    GameStateIndex (..),
    GameStateValue,
    initialGameState,
    
    topFieldRank,
    hidingFieldRank,
    fieldSizeY',
    fieldSizeX',
    fieldArrayIndices,
    criticalArea,
    
    yokokuLv1,
    yokokuLv2,
    yokokuLv3,
    yokokuLv4,
    yokokuLv5,
    yokokuLv6,
    
    makeColor,
    )
    where

-- �Q�[�����̐ݒ蓙�����߂�l
-- �F���E�Ղ�̏����鐔�E������܂Ղ惌�[�g�ȂǂȂǁB�B�B

import qualified Common.DataType              as T
import qualified Common.Function           as U
import qualified Common.Name                 as W

import qualified Data.Vector            as VC
import qualified Data.Vector.Unboxed    as VCU

--------------------------------------------------------------------------------
--  �^
--------------------------------------------------------------------------------
data GameState = GameState GameStateValues      -- Int
                           ColorPattern         -- �F�p�^�[��

type GameStateValues    = VCU.Vector GameStateValue
type GameStateValue     = Int
data GameStateIndex     -- GameState�̒l�̎�ނ�\���B
        = FallTime      -- ��������
        | ErasePuyo     -- �Ղ�̏����鐔
        | Color         -- �Ղ�̐F�̐�
        | OjamaRate     -- ������܂Ղ惌�[�g
        | MarginTime    -- �}�[�W���^�C��
        | FieldSizeY    -- �t�B�[���h�T�C�Y�i�c�j�i���͈͂̃T�C�Y�j
        | FieldSizeX    -- �t�B�[���h�T�C�Y�i���j�i���͈͂̃T�C�Y�j
        | NextPuyoView  -- �l�N�X�g�Ղ�ȍ~�̕\����
        deriving (Show, Eq, Ord, Enum, Bounded)

type ColorPattern   = IO ColorList 
type ColorList      = VC.Vector T.Color

--------------------------------------------------------------------------------
--  ����l
--------------------------------------------------------------------------------
defaultGameState :: GameStateValues
defaultGameState =  VCU.generate numOfGSI $ defaultValue . toEnum

initialGameState    :: GameState
initialGameState    =  GameState defaultGameState 
                                 (randomColorPattern $ limitValue max Color)

-- �e�l�̏����l
defaultValue                :: GameStateIndex -> GameStateValue
defaultValue FallTime       =  40
defaultValue ErasePuyo      =  4
defaultValue Color          =  4
defaultValue OjamaRate      =  70
defaultValue MarginTime     =  96  -- ���b�P�ʂ̐��l
defaultValue FieldSizeY     =  12
defaultValue FieldSizeX     =  6
defaultValue NextPuyoView   =  1

-- ���E��
limitValue f FallTime       = f 1   100
limitValue f ErasePuyo      = f 1   100
limitValue f Color          = f 1   $ fromEnum (maxBound :: T.Color)
limitValue f OjamaRate      = f 1   100
limitValue f MarginTime     = f 1   300
limitValue f FieldSizeY     = f 1   48
limitValue f FieldSizeX     = f 1   24
limitValue f NextPuyoView   = f 0   6

-- GameState�̒l�̎�ށiGameStateIndex�j�̒l�̑����B
numOfGSI    = 1 + fromEnum lastGSI  :: Int

headGSI = minBound  :: GameStateIndex
lastGSI = maxBound  :: GameStateIndex

--------------------------------------------------------------------------------
--  �ǂݎ��
--------------------------------------------------------------------------------
-- �l�����o���B �E�E�E�l�̎�ށiGameStateIndex�j���w�肵�āAGameState������o���B
get :: GameStateIndex -> GameState          -> GameStateValue
get =  \i             -> \(GameState v _)   -> v VCU.! fromEnum i

-- �F�p�^�[��
get_ColorPattern    :: GameState        -> ColorPattern
get_ColorPattern =  \(GameState _ c)    -> c

--------------------------------------------------------------------------------
--  ��������
--------------------------------------------------------------------------------
-- �l������������
newGameState    :: (GameStateValue -> GameStateValue) -> GameStateIndex 
                -> GameState -> GameState
newGameState f i gs@(GameState gv cp)   = GameState gv' cp
  where
    gv' = gv VCU.// [(i', e')]
    i'  = fromEnum i
    e'  | e'' > limitValue max i    = limitValue min i
        | e'' < limitValue min i    = limitValue max i
        | otherwise                 = e''
    e'' = f $ get i gs

--------------------------------------------------------------------------------
--  �t�B�[���h�T�C�Y
--------------------------------------------------------------------------------
-- �Ղ�̉��͈͂̍ŏ�i�̃t�B�[���h�x���W�B
topFieldRank    = 4     :: T.PositionY
-- �Ղ�̉��͈͊O�̌����Ȃ��i�̃t�B�[���h�x���W�B
hidingFieldRank = 3     :: T.PositionY

-- ���͈̓t�B�[���h�T�C�Y �{���̕ǁE�����Ȃ��i�ȏ�̂R�i�B
fieldSizeY' :: GameState -> T.PositionY
fieldSizeY' =  (+) 4 . get FieldSizeY

-- ���͈̓t�B�[���h�T�C�Y �{���E�̕�
fieldSizeX' :: GameState -> T.PositionY
fieldSizeX' =  (+) 2 . get FieldSizeX 

-- �t�B�[���h��Ԃ�ێ������z��̑S�v�f�����X�g�ɂ���B�i�Ղ�̉��͈́j
fieldArrayIndices       :: GameState -> [T.AreaPosition]
fieldArrayIndices gs    = [(y, x) | y <- [topFieldRank..(-1 + fieldSizeY' gs)], 
                                    x <- [2..(-1 + fieldSizeX' gs)] ]

-- �����_
criticalArea    :: GameState -> T.AreaPosition
criticalArea gs =  (topFieldRank, (fieldSizeX' gs + 1) `quot` 2)

--------------------------------------------------------------------------------
--  �\���Ղ�
--------------------------------------------------------------------------------
yokokuLv1    = 1
yokokuLv2 gs    | fieldSizeX > 1    = yokokuLv1 * fieldSizeX
                | otherwise         = 2
  where fieldSizeX = get FieldSizeX gs
yokokuLv3 gs    | fieldSizeX > 1    = W.sizeYyokokuLv3 * yokokuLv2 gs
                | otherwise         = 3
  where fieldSizeX = get FieldSizeX gs
yokokuLv4 gs    | fieldSizeX > 1    = fieldSizeX * yokokuLv3 gs
                | otherwise         = 4
  where fieldSizeX = get FieldSizeX gs
yokokuLv5 gs    | fieldSizeX > 1    = 2 * yokokuLv4 gs
                | otherwise         = 5
  where fieldSizeX = get FieldSizeX gs
yokokuLv6 gs    | fieldSizeX > 1    = 2 * yokokuLv5 gs
                | otherwise         = 6
  where fieldSizeX = get FieldSizeX gs

--------------------------------------------------------------------------------
--  �F�p�^�[��
--------------------------------------------------------------------------------
-- �F�����߂�B
makeColor       :: ColorList -> Int -> T.Color
makeColor cs n  =  cs VC.! n

-- �����_���ȐF�p�^�[�������B
randomColorPattern  :: T.NumOfColors -> ColorPattern
randomColorPattern  =  \noc          -> do
    val     <- U.runRandom $ totalPerms noc - 1
    return $ colorP !! val    

-- �F�̏���̗�
colorP  :: [ColorList]
colorP  =  map VC.fromList $ perms $ map toEnum [0..(limitValue max Color - 1)]
  where
    perms           :: [a] -> [[a]]
    perms []        =  [[]]
    perms (x:xs)    = concat $ map (interleave x) (perms xs)
      where
        interleave          :: a -> [a] -> [[a]]
        interleave x []     =  [[x]]
        interleave x (y:ys) = (x : y : ys) : map (y :) (interleave x ys)

totalPerms n    = product [1..n]

-- �F���̑g�ݍ��킹�B�i�A�z�z��̃��X�g�E��{�F�̐ԁE���E�΁E�E���̂T�F�B�j
--colorC  :: Int -> [ColorList]
--colorC  =  map VC.fromList . c (map toEnum [0..(limitValue max Color - 1)])
--colorC  =  map VC.fromList . c [T.Red, T.Green, T.Blue, T.Yellow, T.Purple]
--  where
--    c :: [a] -> Int -> [[a]]    -- �g�ݍ��킹
--    c _ 0       = [[]]
--    c [] _      = []
--    c (x:xs) n  = map (x:) (c xs (n-1)) ++ c xs n
    
-- �F���̑g�ݍ��킹�̑����B
--totalComb   :: Int -> Int
--totalComb n =
--    let maxColor = limitValue max Color
--    in  product [(maxColor - n + 1)..maxColor] `quot` product [1..n]