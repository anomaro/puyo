module State.Player.Query
    (
    -- �v���[���[���ʎq�擾�B
    get_playerIdentity,
    
    -- �Q�[����Ԏ擾
    get_gamePhase,
    
    -- �t�B�[���h�̏�Ԏ擾
    get_fieldStateArea,
    is_neighborSpace,
    
    -- ����Ղ�̏�Ԏ擾
    get_PlayerPuyoExistent,
    get_PlayerPuyoColors,
    get_PlayerPuyoPosition,
    get_PlayerPuyoDirection,
    get_PlayerPuyoFallTime,
    get_PlayerPuyoRotateTime,
    get_PlayerPuyoQuickTurnFlag,
    
    -- �l�N�X�g�Ղ�̐F�擾
    get_nextPuyoColors,
    
    -- ���_�擾
    get_score,
    
    -- �\���Ղ�擾
    get_fallOjamaPuyo,
    get_yokoku,
    
    -- �s�k�t���O
    get_whoWins,
    
    -- ��Ԑ���
    create_playerstate,
    create_nextPuyoState,
    copy_nextPuyoState,
    create_yokokuState,
    create_loseFlagState,
    )
    where

import qualified State.Player.Substance as P'

import Control.Monad
import Control.Applicative
import qualified Data.IORef         as IORF
import qualified Data.Array.IO      as AIO

import qualified State.Setting          as Setting
import qualified Common.PlayerIdentity  as Identity
import qualified Common.Area            as Area
import qualified Common.Direction       as Direction
import Common.Time  (Time)
import qualified Common.Score           as Score
import Common.Color (Color, determine)
import qualified Common.Field           as Field
import qualified Common.Random          as Random (run, list)
import qualified Common.Yokoku          as Yokoku
import qualified Common.Number          as Number
import qualified Common.Phase           as Phase (Game, start)

--------------------------------------------------------------------------------
(<$<) :: Functor f => (a -> b) -> (c -> f a) -> (c -> f b)
f <$< g =  fmap f . g
infixr 1 <$<

readField   :: (AIO.MArray a e m, AIO.Ix i) => i -> a i e -> m e
readField   =  flip AIO.readArray 

--------------------------------------------------------------------------------
--  �v���C���[��Ԃ̓��e���O�����W���[���ɓ`����B
--------------------------------------------------------------------------------
-- �v���C���[���ʂ�`����B
get_playerIdentity  :: P'.PlayerState -> Identity.PlayerIdentity
get_playerIdentity  =  P'.takeout_playerIdentity

-- �Q�[����Ԃ�`����B
get_gamePhase   :: P'.PlayerState -> IO Phase.Game
get_gamePhase   =  IORF.readIORef <=< P'.takeout_gamePhaseState

-- �l�N�X�g�Ղ�̐F��`����B
get_nextPuyoColors  :: P'.PlayerState -> IO [Color]
get_nextPuyoColors  =  IORF.readIORef <=< P'.takeout_nextPuyoState

--------------------------------------------------------------------------------
--  �s�k�t���O
--------------------------------------------------------------------------------
-- ���������v���C���[�𒲂ׂ�B
get_whoWins         :: P'.PlayerState -> IO (Maybe Identity.Territory)
get_whoWins state   =  do
    loseFlag    <- IORF.readIORef $ P'.takeout_loseFlagState state
    return $ check loseFlag
  where
    check (False, True)     = Just Identity.Left
    check (True,  False)    = Just Identity.Right
    check _                 = Nothing

--------------------------------------------------------------------------------
--  �\���Ղ�
--------------------------------------------------------------------------------
-- ���ۂɍ~�邨����܂Ղ�̐���`����B
get_fallOjamaPuyo   :: Identity.Territory -> P'.PlayerState -> IO Number.Puyo
get_fallOjamaPuyo trt state = do
    yokokuField <- IORF.readIORef $ P'.takeout_yokokuState state
    return $ Yokoku.actual $ (Identity.pick trt) yokokuField

-- �\���Ղ�̐���`����B
get_yokoku  :: Identity.Territory -> P'.PlayerState -> IO Number.Puyo
get_yokoku trt state =  do
    yokokuField <- IORF.readIORef $ P'.takeout_yokokuState state
    return $ Yokoku.total $ (Identity.pick trt) yokokuField
    
--------------------------------------------------------------------------------
--  ���_
--------------------------------------------------------------------------------
-- ���_��`����B
get_score       :: P'.PlayerState -> IO Score.Score
get_score       =  IORF.readIORef . P'.takeout_scoreState

--------------------------------------------------------------------------------
--  �t�B�[���h�̏�Ԏ擾
--------------------------------------------------------------------------------
-- �w�肵���G���A�̃t�B�[���h�̃I�u�W�F�N�g�̎�ނ�`����B
get_fieldStateArea      :: Field.Position -> P'.PlayerState -> IO Area.Area
get_fieldStateArea p    =  readField p <=< P'.takeout_fieldstate
                             
-- �w�肵�������ɗאڂ���G���A�̃I�u�W�F�N�g���A�󔒂��ǂ�������B
is_neighborSpace :: Field.Position -> Direction.Area -> P'.PlayerState -> IO Bool
is_neighborSpace p d =
    Area.isSpace <$< readField (Field.neighbor d p) <=< P'.takeout_fieldstate

--------------------------------------------------------------------------------
--  ����Ղ�̏�Ԏ擾
--------------------------------------------------------------------------------
-- ����Ղ悪���݂��邩�ǂ����`����B
get_PlayerPuyoExistent  :: P'.PlayerState -> IO Bool
get_PlayerPuyoExistent  =  (P'.NonExistent /=) <$< readTakePP

-- ����Ղ�̐F�̑g�݂�`����B
get_PlayerPuyoColors        :: P'.PlayerState -> IO (Color, Color)
get_PlayerPuyoColors        =  getPPColor           <$< readTakePP

-- ����Ղ�̊�_�Ղ�̃t�B�[���h���W��`����B
get_PlayerPuyoPosition      :: P'.PlayerState -> IO Field.Position
get_PlayerPuyoPosition      =  getPPPosition        <$< readTakePP

-- ����Ղ�̓��_�Ղ�̕�����`����B
get_PlayerPuyoDirection     :: P'.PlayerState -> IO Direction.Area
get_PlayerPuyoDirection     =  getPPDirection       <$< readTakePP

-- ����Ղ�̎��R�����p�̃J�E���^��`����B
get_PlayerPuyoFallTime      :: P'.PlayerState -> IO Time
get_PlayerPuyoFallTime      =  getPPFallTime        <$< readTakePP

-- ����Ղ�̉�]�p�̃J�E���^��`����B
get_PlayerPuyoRotateTime    :: P'.PlayerState -> IO Time
get_PlayerPuyoRotateTime    =  getPPRotateTime      <$< readTakePP

-- ����Ղ�̃N�C�b�N�^�[���̉ۂ�`����B
get_PlayerPuyoQuickTurnFlag :: P'.PlayerState -> IO Bool
get_PlayerPuyoQuickTurnFlag =  getPPFlagQuickTurn   <$< readTakePP


readTakePP :: P'.PlayerState -> IO P'.PlayerPuyo
readTakePP =  IORF.readIORef <=< P'.takeout_ppuyostate

getPPColor          (P'.PlayerPuyoInfo c _ _ _ _ _)    = c :: (Color, Color)
getPPPosition       (P'.PlayerPuyoInfo _ p _ _ _ _)    = p :: Field.Position
getPPDirection      (P'.PlayerPuyoInfo _ _ d _ _ _)    = d :: Direction.Area
getPPFallTime       (P'.PlayerPuyoInfo _ _ _ f _ _)    = f :: Time
getPPRotateTime     (P'.PlayerPuyoInfo _ _ _ _ r _)    = r :: Time
getPPFlagQuickTurn  (P'.PlayerPuyoInfo _ _ _ _ _ q)    = q :: Bool


--------------------------------------------------------------------------------
--  �v���C���[��ԏ�����
--------------------------------------------------------------------------------
create_playerstate      :: Identity.PlayerIdentity
                        -> Setting.Setting -> P'.NextPuyoState
                        -> P'.YokokuState -> P'.LoseFlagState
                        -> IO P'.PlayerState
create_playerstate pI gs stateN stateY stateL   =  do
    stateG  <- create_gamePhaseState
    stateF  <- create_fieldstate
    stateP  <- create_ppuyostate
    stateS  <- create_scorestate
    return $ P'.PlayerState pI stateG stateF stateP stateN stateS stateY stateL
  where
    -- ���_�̏������
    create_scorestate   =  IORF.newIORef Score.initial
    -- �Q�[����Ԃ��������B
    create_gamePhaseState   :: IO P'.GamePhaseState
    create_gamePhaseState   =  IORF.newIORef Phase.start
    -- ����Ղ�̏�ԕϐ����������B
    create_ppuyostate   :: IO P'.PlayerPuyoState
    create_ppuyostate   =  IORF.newIORef P'.NonExistent
    -- �t�B�[���h��Ԃ̉ϔz����쐬����B
    create_fieldstate :: IO P'.FieldState
    create_fieldstate = 
        AIO.newArray ((1, 1), (Field.sizeRank gs, Field.sizeLine gs)) Area.initialSnd
        >>= \stateF -> build_wall stateF    -- �ǃI�u�W�F�N�g�����B
        >> return stateF
      where
        build_wall          :: P'.FieldState -> IO()
        build_wall stateF   =  do  
            writeStroke_field stateF walls (fieldArray_indicesY $ Field.sizeRank gs)
            writeStroke_field stateF walls (fieldArray_indicesX $ Field.sizeLine gs)
            writeStroke_field stateF walls (fieldArray_indicesX 1)
          where
            walls = repeat Area.initialFst
            -- �t�B�[���h�́A���X�g���瓾�����W�����̃I�u�W�F�N�g�ɏ���������B
            writeStroke_field :: P'.FieldState -> [Area.Area] -> [Field.Position] -> IO()
            writeStroke_field _      _      []      = return ()
            writeStroke_field _      []     _       = return ()
            writeStroke_field stateF (a:as) (p:ps)  =
                AIO.writeArray stateF p a
                >> writeStroke_field stateF as ps
            -- �t�B�[���h��Ԕz��́AY���W���w�肵���v�f�̃��X�g�B�iX���W�͑���Ղ�̉��͈́j
            fieldArray_indicesY :: Field.Rank -> [Field.Position]
            fieldArray_indicesY y   
             | 1 <= y && y <= Field.sizeRank gs  = [(y, x) | x <- [2..Field.sizeLine gs - 1] ]
             | otherwise                         = []
            -- �t�B�[���h��Ԕz��́AX���W���w�肵���v�f�̃��X�g�B
            fieldArray_indicesX :: Field.Rank -> [Field.Position]
            fieldArray_indicesX x   
             | 1 <= x && x <= Field.sizeLine gs  = [(y, x) | y <- [1..Field.sizeRank gs] ]
             | otherwise                         = []


-- �l�N�X�g�Ղ���������B
create_nextPuyoState        :: Setting.Setting -> IO P'.NextPuyoState
create_nextPuyoState  gs    =  do
    seed    <- Random.run maxBound
    colors  <- Setting.getColorPattern gs
    IORF.newIORef $ map (determine colors)
        $ Random.list (Setting.get Setting.Color gs - 1) seed
        
-- �l�N�X�g�Ղ���R�s�[����B�i�Q�o���̃l�N�X�g�Ղ��Ԃ����Ƃ��Ɏg���B�j
copy_nextPuyoState :: P'.NextPuyoState -> IO P'.NextPuyoState
copy_nextPuyoState =  IORF.newIORef <=< IORF.readIORef  


-- �\���Ղ���������B
create_yokokuState :: IO P'.YokokuState
create_yokokuState =  IORF.newIORef (Yokoku.initial, Yokoku.initial)

-- �s�k�t���O���������B
create_loseFlagState    :: IO P'.LoseFlagState
create_loseFlagState    =  IORF.newIORef (False, False)