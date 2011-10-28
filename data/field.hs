module Data.Field
where

import qualified Data.Direction     as Direction
import qualified Data.Setting       as Setting

--------------------------------------------------------------------------------
--  型
-------------------------------------------------------------------------------- 
type Position   = (Rank, Line)
type Rank       = Int   -- Ｙ座標
type Line       = Int   -- Ｘ座標

--------------------------------------------------------------------------------
--  名前
-------------------------------------------------------------------------------- 
-- 隕石ぷよのおじゃまぷよの予告段数。
rankInseki  = 5         :: Rank

-- ぷよの可視範囲の最上段のフィールドＹ座標。
topRank     = 4         :: Rank

-- ぷよの可視範囲外の見えない段のフィールドＹ座標。
hidingBottomRank = 3         :: Rank

--------------------------------------------------------------------------------
--  関数
-------------------------------------------------------------------------------- 
-- 指定した方向に隣接するフィールド座標を得る。（境界チェックをしない）
-- ゲーム環境を知らない関数が、フィールド可視範囲内でこの関数を使用場合に使う。
neighbor :: Direction.Area -> Position -> Position
neighbor Direction.Up    (y, x)   = (y - 1, x    )
neighbor Direction.Right (y, x)   = (y    , x + 1)
neighbor Direction.Down  (y, x)   = (y + 1, x    )
neighbor Direction.Left  (y, x)   = (y    , x - 1)


-- 可視範囲フィールドサイズ ＋下の壁・見えない段以上の３段。
sizeRank    :: Setting.Setting -> Rank
sizeRank    =  (+) 4 . Setting.get Setting.FieldSizeY

-- 可視範囲フィールドサイズ ＋左右の壁
sizeLine    :: Setting.Setting -> Line
sizeLine    =  (+) 2 . Setting.get Setting.FieldSizeX 


-- フィールド状態を保持した配列の全要素をリストにする。（ぷよの可視範囲）
arrayIndices        :: Setting.Setting -> [Position]
arrayIndices gs     = [(y, x) | y <- [topRank .. (-1 + sizeRank gs)]
                              , x <- [2 ..       (-1 + sizeLine gs)] ]

-- 窒息点
critical    :: Setting.Setting -> Position
critical gs =  (topRank, (sizeLine gs + 1) `quot` 2)