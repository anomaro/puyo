-- erasePhase.hs
module Process.Phase.Erase
    (
    erase_puyo,
    rewriteSpase_puyo,
    )
    where

import qualified Control.Monad      as MND

import qualified Common.PlayerIdentity  as Identity (against, territory)
import qualified Common.Area            as Area

import qualified Common.DataType   as T
import qualified Common.Function    as U
import qualified State.Setting  as V
import qualified Common.Name      as W

import State.Player.DataType
import State.Player.Query
import State.Player.Overwriting

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- 結合状態を調べてぷよを消滅させる。ぷよが消えた場合True
erase_puyo          :: V.GameState -> PlayerState -> IO Bool
erase_puyo gs state =  do
    renewScoreCalculation (1+) (const []) (const []) state
    MND.foldM f False $ V.fieldArrayIndices gs
  where
    f :: Bool -> T.AreaPosition -> IO Bool
    f b p = do
        area  <- get_fieldStateArea p state 
        numUnion    <- if isTarget area T.AnyColor p Area.NotYet
                         then check_union state p
                         else return 0
        bool        <- if numUnion < V.get V.ErasePuyo gs
                         then return False
                         else do
                            Area.Puyo c _ _  <- get_fieldStateArea p state
                            erase_unionPuyo state p
                            renewScoreCalculation id (numUnion:) (c:) state 
                            return True
        off_unionCheck state p
        return $ b || bool

-- 結合状態を調べてぷよを消滅させる。（消滅フラグが立っているエリアを空白にする）
rewriteSpase_puyo           :: V.GameState -> PlayerState -> IO ()
rewriteSpase_puyo gs state  =  do
    MND.mapM_ f $ V.fieldArrayIndices gs
    calculateScoreD state   -- 得点を計算して、DynamicScoreを書き換える。
    calculateYokoku
  where
    f :: T.AreaPosition -> IO()
    f p = do
        area    <- get_fieldStateArea p state
        MND.when (isTarget area T.AnyColor p Area.EraseFlag)
                 $ renew_fieldArea state p Area.Space
    -- 予告ぷよを算出する。
    calculateYokoku =  do
        T.Score ss ds <- get_score state
        let (n, m)  = ds `quotRem` V.get V.OjamaRate gs
        renewYokoku id id (n +) trt state
        renewScore ((ds - m) +) (const m) state
      where
        trt = Identity.against . Identity.territory $ get_playerIdentity state

--------------------------------------------------------------------------------
-- 結合チェック状態を未調査状態にする。
off_unionCheck :: PlayerState -> T.AreaPosition -> IO()
off_unionCheck state p = do
    area <- get_fieldStateArea p state
    MND.when (isTarget area T.AnyColor p Area.Completion)
        $ rewrite_unionCheck state p Area.NotYet

-- 結合しているぷよを消滅させる。（消滅フラグを立てる。）
erase_unionPuyo :: PlayerState -> T.AreaPosition -> IO()
erase_unionPuyo state p = do
    Area.Puyo color _ _  <- get_fieldStateArea p state
    renew_fieldArea state p $ Area.Puyo color Area.EraseFlag Area.animeStartErasing
    mapM_ (fff color) listDirection
  where
    fff :: T.Color -> T.Direction -> IO()
    fff color d = do
        area    <- get_fieldStateArea p' state
        erase_ojamaPuyo area state p'
        MND.when (isTarget area color p' Area.Completion)
          $ erase_unionPuyo state p'
      where
        p'  = U.neighbor_area d p

-- おじゃまぷよを消滅させる。（消滅フラグを立てる。）
erase_ojamaPuyo :: Area.Area -> PlayerState -> T.AreaPosition -> IO()
erase_ojamaPuyo (Area.Ojama Area.NotYet _) state p    =
    renew_fieldArea state p $ Area.Ojama Area.EraseFlag Area.animeStartErasing
erase_ojamaPuyo _                    _     _    = return ()

-- エリア対象の結合チェック。このエリアがチェック対象かどうかは事前に調べてある。
check_union :: PlayerState -> T.AreaPosition -> IO T.NumOfUnion
check_union state p    = do
    Area.Puyo color _ _  <- get_fieldStateArea p state
    rewrite_unionCheck state p Area.Completion
    MND.foldM (fff color) 1 listDirection
  where
    fff :: T.Color -> T.NumOfUnion -> T.Direction -> IO T.NumOfUnion
    fff color n d = do
        area    <- get_fieldStateArea p' state
        if isTarget area color p' Area.NotYet
          then check_union state p' >>= return . (+ n)
          else return n
      where
        p'  = U.neighbor_area d p

-- 対象のエリアが結合チェック・ぷよ消滅の対象かどうか判定。
isTarget :: Area.Area -> T.Color -> T.AreaPosition -> Area.UnionCheck -> Bool
isTarget (Area.Puyo c uc _)          c'         (y, _) uc'
  | y >= V.topFieldRank && uc == uc'    = c' == T.AnyColor || c == c'
isTarget (Area.Ojama  Area.EraseFlag _) T.AnyColor (y, _) Area.EraseFlag
                                        = y >= V.topFieldRank
isTarget _ _ _ _                        = False


--結合調査状態の書き換え。（色ぷよのエリアを対象に使用する）
rewrite_unionCheck :: PlayerState -> T.AreaPosition -> Area.UnionCheck -> IO()
rewrite_unionCheck state p uc  = do
    Area.Puyo color _ at <- get_fieldStateArea p state
    renew_fieldArea state p $ Area.Puyo color uc at
    
-- 方向のリスト
listDirection   = [T.DUp, T.DRight, T.DDown, T.DLeft]