{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Types.Entity.Entity where

import Parsing.ParseUtils.Serialize
import Types.World.World
import Types.Entity.Broadcast
import Types.Logic.Game (DeltaTime)
import qualified Data.Map.Strict as Map

data EntityData = EntityData {
    hp :: Int,
    pos :: Coord
}
data EntitySight = EntitySight [Tile] [EntityWrapper]

class (Serializable e) => Entity e where
    updateE :: DeltaTime -> [Event] -> [Broadcast] -> EntitySight -> e -> (e, [Broadcast], [Action])
    getDataE :: e -> EntityData

data EntityWrapper = forall e. (Entity e) => EntityWrapper e

update :: DeltaTime -> [Event] -> [Broadcast] -> EntitySight -> EntityWrapper -> (EntityWrapper, [Broadcast], [Action])
update a b c d (EntityWrapper e) = let (e', f, g) = updateE a b c d e in
    (EntityWrapper e', f, g)

getData :: EntityWrapper -> EntityData
getData (EntityWrapper e) = getDataE e

type RegionEntityTable = RegionMap [EntityWrapper]