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
--  ボタンの状態
--------------------------------------------------------------------------------
-- ボタン状態。
data ButtonState
    = ButtonState InputState LastInputState ActualState (ModeState, ModeState)

-- 押しっぱなし入力を防ぐ。入力を受け付ける状態と、拒否する状態、入力を受け付け続ける状態。
type ModeState      = IORF.IORef ButtonMode
data ButtonMode     = Acceptance | Refusal Time.Time | Lifting
        deriving (Show, Eq)

-- 入力されたボタンの種類を格納して置く。
type ActualState    = IORF.IORef [Button]   -- 外部に伝えるボタンの状態。
type InputState     = IORF.IORef [Button]   -- 入力されたキー。
type LastInputState = IORF.IORef [Button]   -- 前回入力されたキー。
type Button         = GLUT.Key


--------------------------------------------------------------------------------
--  ボタンの状態の操作
--------------------------------------------------------------------------------
-- ボタン状態を生成。
create_buttonState  :: IO ButtonState
create_buttonState  =  do
    listIB  <- IORF.newIORef []
    listLB  <- IORF.newIORef []
    listAB  <- IORF.newIORef []
    modeL   <- IORF.newIORef Acceptance
    modeR   <- IORF.newIORef Acceptance
    return $ ButtonState listIB listLB listAB (modeL, modeR)

-- ボタン状態を読み取る。
read_buttonState        :: ButtonState -> IO [Button]
read_buttonState (ButtonState _ _ listAB _)    = IORF.readIORef listAB

-- ボタン状態へ新たなキーを格納する。
putIn_key   :: ButtonState -> GLUT.Key -> IO()
putIn_key (ButtonState listIB _ _ _) key
    | is_validButton key    = IORF.modifyIORef listIB ( nub.(++[key]) )
    | otherwise             = return ()

-- ボタン状態からキーを除外する。
putOut_key   :: ButtonState -> GLUT.Key -> IO()
putOut_key (ButtonState listIB _ _ _) key
    | is_validButton key    = IORF.modifyIORef listIB ( filter (/=key) )
    | otherwise             = return ()

-- ボタン状態を更新。（タイマーコールバックで処理。）
renew_buttonState   :: ButtonState -> IO()
renew_buttonState ( ButtonState listIB listLB listAB (modeL, modeR) )   = do
    listIB'  <- IORF.readIORef listIB
    listLB'  <- IORF.readIORef listLB
    IORF.writeIORef listAB listIB'
    
    -- 押しっぱなし禁止ボタン
    MND.when (elem_Button list_banButton listIB' 
              && elem_Button list_banButton listLB')
             $ IORF.modifyIORef listAB (\\ list_banButton)
    
    -- 押しっぱなし制限ボタン
    limit left_button  listIB' listLB' modeL
    limit right_button listIB' listLB' modeR
    
    IORF.writeIORef listLB listIB'
  where
    limit :: Button -> [Button] -> [Button] -> ModeState -> IO()
    limit button listIB' listLB' mode = do
        mode'    <- IORF.readIORef mode
        if not $ elem button listIB'
          then IORF.writeIORef mode Acceptance    -- 制限解除
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
--  キー
--------------------------------------------------------------------------------
-- キーとボタンの対応。
up_button       = GLUT.SpecialKey GLUT.KeyUp    :: Button
right_button    = GLUT.SpecialKey GLUT.KeyRight :: Button
down_button     = GLUT.SpecialKey GLUT.KeyDown  :: Button
left_button     = GLUT.SpecialKey GLUT.KeyLeft  :: Button
one_button      = GLUT.Char 'z'                 :: Button
two_button      = GLUT.Char 'x'                 :: Button

-- 押しっぱなし禁止ボタン
list_banButton      = [one_button, two_button]      :: [Button]
-- 押しっぱなし制限ボタン
list_limitButton    = [right_button, left_button]   :: [Button]

-- ゲームで有効なボタンかどうか判定。
is_validButton      :: GLUT.Key -> Bool
is_validButton k    | k == up_button    = True
                    | k == right_button = True
                    | k == down_button  = True
                    | k == left_button  = True
                    | k == one_button   = True
                    | k == two_button   = True
                    | otherwise         = False
                    
-- 特定のボタンがあるどうか判定。
elem_Button               :: [Button] -> [Button] -> Bool
elem_Button listB listB'  =  foldr (\b -> ( elem b listB' || ) ) False listB



testButtonList  = [GLUT.SpecialKey GLUT.KeyRight]