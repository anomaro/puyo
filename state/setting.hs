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

-- ゲーム環境の設定等を決める値
-- 色数・ぷよの消える数・おじゃまぷよレートなどなど。。。

import qualified Common.DataType              as T
import qualified Common.Function           as U
import qualified Common.Name                 as W

import qualified Data.Vector            as VC
import qualified Data.Vector.Unboxed    as VCU

--------------------------------------------------------------------------------
--  型
--------------------------------------------------------------------------------
data GameState = GameState GameStateValues      -- Int
                           ColorPattern         -- 色パターン

type GameStateValues    = VCU.Vector GameStateValue
type GameStateValue     = Int
data GameStateIndex     -- GameStateの値の種類を表す。
        = FallTime      -- 落下時間
        | ErasePuyo     -- ぷよの消える数
        | Color         -- ぷよの色の数
        | OjamaRate     -- おじゃまぷよレート
        | MarginTime    -- マージンタイム
        | FieldSizeY    -- フィールドサイズ（縦）（可視範囲のサイズ）
        | FieldSizeX    -- フィールドサイズ（横）（可視範囲のサイズ）
        | NextPuyoView  -- ネクストぷよ以降の表示数
        deriving (Show, Eq, Ord, Enum, Bounded)

type ColorPattern   = IO ColorList 
type ColorList      = VC.Vector T.Color

--------------------------------------------------------------------------------
--  既定値
--------------------------------------------------------------------------------
defaultGameState :: GameStateValues
defaultGameState =  VCU.generate numOfGSI $ defaultValue . toEnum

initialGameState    :: GameState
initialGameState    =  GameState defaultGameState 
                                 (randomColorPattern $ limitValue max Color)

-- 各値の初期値
defaultValue                :: GameStateIndex -> GameStateValue
defaultValue FallTime       =  40
defaultValue ErasePuyo      =  4
defaultValue Color          =  4
defaultValue OjamaRate      =  70
defaultValue MarginTime     =  96  -- ※秒単位の数値
defaultValue FieldSizeY     =  12
defaultValue FieldSizeX     =  6
defaultValue NextPuyoView   =  1

-- 境界数
limitValue f FallTime       = f 1   100
limitValue f ErasePuyo      = f 1   100
limitValue f Color          = f 1   $ fromEnum (maxBound :: T.Color)
limitValue f OjamaRate      = f 1   100
limitValue f MarginTime     = f 1   300
limitValue f FieldSizeY     = f 1   48
limitValue f FieldSizeX     = f 1   24
limitValue f NextPuyoView   = f 0   6

-- GameStateの値の種類（GameStateIndex）の値の総数。
numOfGSI    = 1 + fromEnum lastGSI  :: Int

headGSI = minBound  :: GameStateIndex
lastGSI = maxBound  :: GameStateIndex

--------------------------------------------------------------------------------
--  読み取り
--------------------------------------------------------------------------------
-- 値を取り出す。 ・・・値の種類（GameStateIndex）を指定して、GameStateから取り出す。
get :: GameStateIndex -> GameState          -> GameStateValue
get =  \i             -> \(GameState v _)   -> v VCU.! fromEnum i

-- 色パターン
get_ColorPattern    :: GameState        -> ColorPattern
get_ColorPattern =  \(GameState _ c)    -> c

--------------------------------------------------------------------------------
--  書き換え
--------------------------------------------------------------------------------
-- 値を書き換える
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
--  フィールドサイズ
--------------------------------------------------------------------------------
-- ぷよの可視範囲の最上段のフィールドＹ座標。
topFieldRank    = 4     :: T.PositionY
-- ぷよの可視範囲外の見えない段のフィールドＹ座標。
hidingFieldRank = 3     :: T.PositionY

-- 可視範囲フィールドサイズ ＋下の壁・見えない段以上の３段。
fieldSizeY' :: GameState -> T.PositionY
fieldSizeY' =  (+) 4 . get FieldSizeY

-- 可視範囲フィールドサイズ ＋左右の壁
fieldSizeX' :: GameState -> T.PositionY
fieldSizeX' =  (+) 2 . get FieldSizeX 

-- フィールド状態を保持した配列の全要素をリストにする。（ぷよの可視範囲）
fieldArrayIndices       :: GameState -> [T.AreaPosition]
fieldArrayIndices gs    = [(y, x) | y <- [topFieldRank..(-1 + fieldSizeY' gs)], 
                                    x <- [2..(-1 + fieldSizeX' gs)] ]

-- 窒息点
criticalArea    :: GameState -> T.AreaPosition
criticalArea gs =  (topFieldRank, (fieldSizeX' gs + 1) `quot` 2)

--------------------------------------------------------------------------------
--  予告ぷよ
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
--  色パターン
--------------------------------------------------------------------------------
-- 色を決める。
makeColor       :: ColorList -> Int -> T.Color
makeColor cs n  =  cs VC.! n

-- ランダムな色パターンを作る。
randomColorPattern  :: T.NumOfColors -> ColorPattern
randomColorPattern  =  \noc          -> do
    val     <- U.runRandom $ totalPerms noc - 1
    return $ colorP !! val    

-- 色の順列の列挙
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

-- 色数の組み合わせ。（連想配列のリスト・基本色の赤・黄・緑・青・紫の５色。）
--colorC  :: Int -> [ColorList]
--colorC  =  map VC.fromList . c (map toEnum [0..(limitValue max Color - 1)])
--colorC  =  map VC.fromList . c [T.Red, T.Green, T.Blue, T.Yellow, T.Purple]
--  where
--    c :: [a] -> Int -> [[a]]    -- 組み合わせ
--    c _ 0       = [[]]
--    c [] _      = []
--    c (x:xs) n  = map (x:) (c xs (n-1)) ++ c xs n
    
-- 色数の組み合わせの総数。
--totalComb   :: Int -> Int
--totalComb n =
--    let maxColor = limitValue max Color
--    in  product [(maxColor - n + 1)..maxColor] `quot` product [1..n]