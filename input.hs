-- file: input.hs
module Input
    (
    ButtonState,
    
    Button,
    up_button,
    right_button,
    down_button,
    left_button,
    one_button,
    two_button,
    read_buttonState,
    
    create_buttonState,
    putIn_key,
    putOut_key,
    renew_buttonState,
    
    
    testButtonList,
    )
    where

import qualified Control.Monad      as MND
import qualified Data.IORef         as IORF
import qualified Graphics.UI.GLUT   as GLUT
import Data.List (nub, (\\))

import qualified Data.Time        as Time (Time, inputMoveX, count)

--------------------------------------------------------------------------------
--  �{�^���̏��
--------------------------------------------------------------------------------
-- �{�^����ԁB
data ButtonState
    = ButtonState InputState LastInputState ActualState (ModeState, ModeState)

-- �������ςȂ����͂�h���B���͂��󂯕t�����ԂƁA���ۂ����ԁA���͂��󂯕t���������ԁB
type ModeState      = IORF.IORef ButtonMode
data ButtonMode     = Acceptance | Refusal Time.Time | Lifting
        deriving (Show, Eq)

-- ���͂��ꂽ�{�^���̎�ނ��i�[���Ēu���B
type ActualState    = IORF.IORef [Button]   -- �O���ɓ`����{�^���̏�ԁB
type InputState     = IORF.IORef [Button]   -- ���͂��ꂽ�L�[�B
type LastInputState = IORF.IORef [Button]   -- �O����͂��ꂽ�L�[�B
type Button         = GLUT.Key


--------------------------------------------------------------------------------
--  �{�^���̏�Ԃ̑���
--------------------------------------------------------------------------------
-- �{�^����Ԃ𐶐��B
create_buttonState  :: IO ButtonState
create_buttonState  =  do
    listIB  <- IORF.newIORef []
    listLB  <- IORF.newIORef []
    listAB  <- IORF.newIORef []
    modeL   <- IORF.newIORef Acceptance
    modeR   <- IORF.newIORef Acceptance
    return $ ButtonState listIB listLB listAB (modeL, modeR)

-- �{�^����Ԃ�ǂݎ��B
read_buttonState        :: ButtonState -> IO [Button]
read_buttonState (ButtonState _ _ listAB _)    = IORF.readIORef listAB

-- �{�^����Ԃ֐V���ȃL�[���i�[����B
putIn_key   :: ButtonState -> GLUT.Key -> IO()
putIn_key (ButtonState listIB _ _ _) key
    | is_validButton key    = IORF.modifyIORef listIB ( nub.(++[key]) )
    | otherwise             = return ()

-- �{�^����Ԃ���L�[�����O����B
putOut_key   :: ButtonState -> GLUT.Key -> IO()
putOut_key (ButtonState listIB _ _ _) key
    | is_validButton key    = IORF.modifyIORef listIB ( filter (/=key) )
    | otherwise             = return ()

-- �{�^����Ԃ��X�V�B�i�^�C�}�[�R�[���o�b�N�ŏ����B�j
renew_buttonState   :: ButtonState -> IO()
renew_buttonState ( ButtonState listIB listLB listAB (modeL, modeR) )   = do
    listIB'  <- IORF.readIORef listIB
    listLB'  <- IORF.readIORef listLB
    IORF.writeIORef listAB listIB'
    
    -- �������ςȂ��֎~�{�^��
    MND.when (elem_Button list_banButton listIB' 
              && elem_Button list_banButton listLB')
             $ IORF.modifyIORef listAB (\\ list_banButton)
    
    -- �������ςȂ������{�^��
    limit left_button  listIB' listLB' modeL
    limit right_button listIB' listLB' modeR
    
    IORF.writeIORef listLB listIB'
  where
    limit :: Button -> [Button] -> [Button] -> ModeState -> IO()
    limit button listIB' listLB' mode = do
        mode'    <- IORF.readIORef mode
        if not $ elem button listIB'
          then IORF.writeIORef mode Acceptance    -- ��������
          else MND.when (elem_Button list_limitButton listLB') $
                 case mode'
                   of Acceptance
                        -> IORF.writeIORef mode (Refusal Time.inputMoveX)
                      (Refusal 0) 
                        -> IORF.writeIORef mode Lifting
                      (Refusal n)
                        -> do IORF.writeIORef mode (Refusal $ Time.count n)
                              IORF.modifyIORef listAB (\\ [button])
                      _ -> return ()
                      
--------------------------------------------------------------------------------
--  �L�[
--------------------------------------------------------------------------------
-- �L�[�ƃ{�^���̑Ή��B
up_button       = GLUT.SpecialKey GLUT.KeyUp    :: Button
right_button    = GLUT.SpecialKey GLUT.KeyRight :: Button
down_button     = GLUT.SpecialKey GLUT.KeyDown  :: Button
left_button     = GLUT.SpecialKey GLUT.KeyLeft  :: Button
one_button      = GLUT.Char 'z'                 :: Button
two_button      = GLUT.Char 'x'                 :: Button

-- �������ςȂ��֎~�{�^��
list_banButton      = [one_button, two_button]      :: [Button]
-- �������ςȂ������{�^��
list_limitButton    = [right_button, left_button]   :: [Button]

-- �Q�[���ŗL���ȃ{�^�����ǂ�������B
is_validButton      :: GLUT.Key -> Bool
is_validButton k    | k == up_button    = True
                    | k == right_button = True
                    | k == down_button  = True
                    | k == left_button  = True
                    | k == one_button   = True
                    | k == two_button   = True
                    | otherwise         = False
                    
-- ����̃{�^��������ǂ�������B
elem_Button               :: [Button] -> [Button] -> Bool
elem_Button listB listB'  =  foldr (\b -> ( elem b listB' || ) ) False listB



testButtonList  = [GLUT.SpecialKey GLUT.KeyRight]