-- file: typedata.hs
module Common.DataType
     where


--------------------------------------------------------------------------------
--  場面遷移
--------------------------------------------------------------------------------
data Period = Game GameName -- ゲーム（ぷよぷよ）
            | Configuration -- ゲームの設定

data GameName   = PuyoPuyo  -- 通常のぷよぷよ

--------------------------------------------------------------------------------
--  ゲーム
--------------------------------------------------------------------------------
-- 時間
type Time   = Int
    
-- フィールド座標。( Ｙ座標, Ｘ座標 )
type AreaPosition   = (PositionY, PositionX)
type PositionY  = Int
type PositionX  = Int 

-- 方向
data Direction  = DUp | DRight | DDown | DLeft | DPoint
        deriving (Show, Eq)
-- 回転方向
data RotationDirection  = RRight | RLeft | RPoint
        deriving (Show, Eq)
        
-- 色
data Color  = Red 
            | Green
            | Blue 
            | Yellow 
            | Purple 
            | AnyColor
        deriving (Show, Eq, Ord, Enum, Bounded) --            | SkyBlue

-- 数
type NumOfPuyos     = Int   -- 組みぷよの数
type NumOfPuyo      = Int   -- ぷよの数
type NumOfUnion     = Int   -- ぷよの結合数 (ぷよの数と同じ？)
type NumOfColors    = Int   -- 色の数
type NumOfChain     = Int   -- 連鎖数

--------------------------------------------------------------------------------
--  エリア
--------------------------------------------------------------------------------
-- エリア（フィールドの１マス分をそう呼ぶことにする）を構成するオブジェクトの種類。
data Area   = Space
             | Puyo Color UnionCheck AnimationType
--            | Puyo Color NumOfUnion UnionCheck AnimationType
            | Ojama UnionCheck AnimationType
            | Wall
        deriving (Show, Eq)

-- ぷよの結合チェック状態。
data UnionCheck = NotYet        -- 未調査
                | Completion    -- 調査完了
                | EraseFlag     -- 消滅フラグ
        deriving (Show, Eq)
        
-- フィールドぷよのアニメーションの状態を表すデータ。
data AnimationType  = Normal                -- アニメーション無し
                    | Dropping   Time       -- 落下時
                    | Landing    Time Power -- 着地時
                    | Erasing    Time       -- 消滅時
--                    | Projecting Time       -- 消滅予測時
        deriving (Show, Eq)

type Power  = Double        -- アニメーションの強さ。

--------------------------------------------------------------------------------
--  ゲームの状態遷移
--------------------------------------------------------------------------------
-- ゲームの状態遷移を表すデータ。
data GamePhase  = BuildPhase    -- ぷよ生成
                | ControlPhase  -- プレイヤーの操作
                | DropPhase     -- ちぎりやぷよ消滅後に起こるぷよ落下
                | ErasePhase    -- ぷよ消滅
                | ErasePhase'
                | FallPhase     -- おじゃまぷよの落下
                | FallPhase'
                | GameOverPhase -- ゲームオーバー
                | AnimationPhase  Time GamePhase  -- 硬直時間と次の状態遷移
        deriving (Show, Eq)
--------------------------------------------------------------------------------
--  プレイヤー
--------------------------------------------------------------------------------
-- プレイヤーを識別するデータ。
type PlayerIdentity  = (Territory, Player)

-- 表示フィールドを識別するデータ。
data Territory  = TerritoryLeft
                | TerritoryRight
        deriving (Show, Eq)
-- プレイヤーを表すデータ。
data Player     = User          -- ユーザ
                | Com ComName   -- コンピュータ　名前
data ComName    = Pechipechi

--------------------------------------------------------------------------------
--  スコア
--------------------------------------------------------------------------------
data Score  = Score StaticScore DynamicScore
type StaticScore    = ScoreBaseType     -- おじゃまぷよに換算されたスコア
type DynamicScore   = ScoreBaseType     -- おじゃまぷよに換算されていないスコア
type ScoreBaseType  = Int           -- スコアの数値の型。 ※2147483647
