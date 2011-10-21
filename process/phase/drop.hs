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
import qualified Data.Area            as Area
import qualified Data.Direction       as Direction
import qualified Data.Field           as Field
import Data.Setting (Setting)

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
        let pos'@(y',_) = Field.neighbor d pos
        boolb   <- is_neighborSpace pos  Direction.Down state
        boolm   <- is_neighborSpace pos' Direction.Down state
        let (areaB, powerB) = Area.landPuyo cb pos d boolb
            (areaM, powerM) = Area.landPuyo cm pos' (Direction.inversion d) boolm

        MND.when (y  >= Field.hidingBottomRank) $ renew_fieldArea state pos  areaB
        MND.when (y' >= Field.hidingBottomRank) $ renew_fieldArea state pos' areaM
        
        areaB'  <- get_fieldStateArea (Field.neighbor Direction.Down pos ) state
        areaM'  <- get_fieldStateArea (Field.neighbor Direction.Down pos') state
        
        transmitAnimeLanding powerB areaB' (Field.neighbor Direction.Down pos ) state
        transmitAnimeLanding powerM areaM' (Field.neighbor Direction.Down pos') state

--------------------------------------------------------------------------------
-- ちぎり落下
--------------------------------------------------------------------------------
drop_puyo           :: Setting -> PlayerState -> IO Bool
drop_puyo gs state  =
    MND.foldM fff False fieldArray_indices
  where
    -- ちぎり落下の適用範囲。
    fieldArray_indices
        =[(y, x) | x <- [2..(Field.sizeLine gs - 1)],
                   let sizeY = Field.sizeRank gs,
                   y <- [(sizeY - 1), (sizeY - 2) .. Field.hidingBottomRank] ]
    fff     :: Bool -> Field.Position -> IO Bool
    fff b p =  do
        b'  <- drop_areaPuyo  p
        return $ b || b'
      where    
        -- １つのぷよを１エリア分落下させる。接地ぷよからの高さを返す。
        drop_areaPuyo   :: Field.Position -> IO Bool
        drop_areaPuyo p =  do
            areaObj    <- get_fieldStateArea p state 
            flagSpace  <- is_neighborSpace p Direction.Down state
            
            if flagSpace && Area.isPuyo areaObj 
              then do 
                renew_fieldArea state p' areaObj
                renew_fieldArea state p  standard
                flagSpace'  <- is_neighborSpace p' Direction.Down state
                areaObj''   <- get_fieldStateArea p'' state
                if flagSpace' || Area.isDroppingAnime areaObj''
                  then do
                    rewriteAnimation state p' areaObj Area.animeStartDropping
                  else do
                    rewriteAnimation state p' areaObj defaultLanding
                    transmitAnimeLanding Area.defauletPower areaObj'' p'' state   
                return True
              else do
                return False
          where
            p'@(y', _)  = Field.neighbor Direction.Down p
            p'' = Field.neighbor Direction.Down p'

--------------------------------------------------------------------------------
-- アニメーションタイプ
--------------------------------------------------------------------------------
-- アニメーションタイプを書き換える。
rewriteAnimation   :: PlayerState -> Field.Position 
                        -> Area.Area -> Area.AnimationType -> IO()
rewriteAnimation s p a anime
  | Area.isPuyo a   = renew_fieldArea s p $ Area.modifyAnime (const anime) a
  | otherwise       = return ()


-- 落下があった列のぷよをアニメーションさせる。
transmitAnimeLanding    :: Area.Power -> Area.Area -> Field.Position
                        -> PlayerState -> IO()
transmitAnimeLanding 0 _     _ _    = return ()
transmitAnimeLanding a area p state = MND.when (Area.isNoAnime area)
                                        $ tAL a area p state
  where
    tAL a area p state = do
        rewriteAnimation state p area $ Area.animeStartLanding (a - 1)
        area'   <- get_fieldStateArea p' state
        transmitAnimeLanding (a - 1) area' p' state
      where
        p' = Field.neighbor Direction.Down p

defaultLanding  = Area.animeStartLanding Area.defauletPower