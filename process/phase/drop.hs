-- dropPhase.hs
module Process.Phase.Drop
    (
    land_puyo,
    drop_puyo,
    )
    where

import Control.Applicative
import qualified Control.Monad      as MND

import Standardizable

import qualified Common.DataType    as T
import qualified Common.Function    as U
import qualified State.Setting      as V 
import qualified Common.Name        as W

import State.Player.DataType
import State.Player.Query   (
    get_fieldStateArea,
    is_neighborSpace,
    get_PlayerPuyoExistent,
    get_PlayerPuyoColors,
    get_PlayerPuyoPosition,
    get_PlayerPuyoDirection,
    )
import State.Player.Overwriting (
    remove_playerPuyo,
    renew_fieldArea,
    )  

import qualified Common.Area            as Area

--------------------------------------------------------------------------------
-- 操作ぷよの着地
--------------------------------------------------------------------------------
land_puyo       :: PlayerState -> IO()
land_puyo state =  do
    b <- get_PlayerPuyoExistent state
    MND.when b $ add_playerpuyo >> remove_playerPuyo state
  where
    -- 操作ぷよをフィールドの状態に加える。
    add_playerpuyo  :: IO()
    add_playerpuyo  =  do
        (cb, cm)    <- get_PlayerPuyoColors     state
        pos@(y,_)   <- get_PlayerPuyoPosition   state
        d           <- get_PlayerPuyoDirection  state
        let pos'@(y',_) = U.neighbor_area d pos
        boolb   <- is_neighborSpace pos  T.DDown state
        boolm   <- is_neighborSpace pos' T.DDown state
        let powerM = if d == T.DDown then Area.defauletPower - 1
                                     else Area.defauletPower
            powerB = if d == T.DUp   then Area.defauletPower - 1
                                     else Area.defauletPower
            atb = (Area.animeStartLanding powerB) `orStandardIf` not (boolb && d /= T.DDown)
            atm = (Area.animeStartLanding powerM) `orStandardIf` not (boolm && d /= T.DUp)
        
        MND.when (y  >= V.hidingFieldRank)
                 $ renew_fieldArea state pos  $ Area.landPuyo cb atb
        MND.when (y' >= V.hidingFieldRank)
                 $ renew_fieldArea state pos' $ Area.landPuyo cm atm
        
        areaB   <- get_fieldStateArea (U.neighbor_area T.DDown pos ) state
        areaM   <- get_fieldStateArea (U.neighbor_area T.DDown pos') state
        
        transmitAnimeLanding powerB areaB (U.neighbor_area T.DDown pos ) state
        transmitAnimeLanding powerM areaM (U.neighbor_area T.DDown pos') state
  
--------------------------------------------------------------------------------
-- ちぎり落下
--------------------------------------------------------------------------------
drop_puyo           :: V.GameState -> PlayerState -> IO Bool
drop_puyo gs state  =
    MND.foldM fff False fieldArray_indices
  where
    -- ちぎり落下の適用範囲。
    fieldArray_indices
        =[(y, x) | x <- [2..(V.fieldSizeX' gs - 1)],
                   let sizeY = V.fieldSizeY' gs,
                   y <- [(sizeY - 1), (sizeY - 2) .. V.hidingFieldRank] ]
    fff     :: Bool -> T.AreaPosition -> IO Bool
    fff b p =  do
        b'  <- drop_areaPuyo  p
        return $ b || b'
      where    
        -- １つのぷよを１エリア分落下させる。接地ぷよからの高さを返す。
        drop_areaPuyo   :: T.AreaPosition -> IO Bool
        drop_areaPuyo p =  do
            areaObj    <- get_fieldStateArea p state 
            flagSpace  <- is_neighborSpace p T.DDown state
            
            if flagSpace && Area.isPuyo areaObj 
              then do 
                renew_fieldArea state p' areaObj
                renew_fieldArea state p  Area.Space
                flagSpace'  <- is_neighborSpace p' T.DDown state
                areaObj''   <- get_fieldStateArea p'' state
                if flagSpace' || Area.isDroppingAnime areaObj''
                  then do
                    rewrite_animationType state p' areaObj Area.animeStartDropping
                  else do
                    rewrite_animationType state p' areaObj defaultLanding
                    transmitDefault areaObj'' p'' state   
                return True
              else do
                return False
          where
            p'@(y', _)  = U.neighbor_area T.DDown p
            p'' = U.neighbor_area T.DDown p'

--------------------------------------------------------------------------------
-- アニメーションタイプ
--------------------------------------------------------------------------------
-- アニメーションタイプを書き換える。
rewrite_animationType   :: PlayerState -> T.AreaPosition 
                        -> Area.Area -> Area.AnimationType -> IO()
rewrite_animationType state p area at
    | Area.isPuyo area
                = renew_fieldArea state p $ Area.modifyAnime (const at) area
    | otherwise = return ()


-- 落下があった列のぷよをアニメーションさせる。
transmitAnimeLanding    :: Area.Power -> Area.Area -> T.AreaPosition -> PlayerState 
                        -> IO()
transmitAnimeLanding 0 _     _ _    = return ()
transmitAnimeLanding a area p state = MND.when (Area.isNoAnime area)
                                        $ tAL a area p state

tAL a area p state = do
    rewrite_animationType state p area $ Area.animeStartLanding (a - 1)
    area'   <- get_fieldStateArea p' state
    transmitAnimeLanding (a - 1) area' p' state
  where
    p' = U.neighbor_area T.DDown p

-- transmitAnimeLandingの標準化
transmitDefault :: Area.Area -> T.AreaPosition -> PlayerState -> IO()
transmitDefault =  transmitAnimeLanding Area.defauletPower

defaultLanding  = Area.animeStartLanding Area.defauletPower