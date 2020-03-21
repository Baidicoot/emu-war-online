{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Types.Entity.Entity where

import Parsing.ParseUtils.Serialize
import Types.World.World
import Types.Entity.Broadcasts

data EntityData = EntityData Int
data EntitySight = EntitySight [Tile] [EntityWrapper]

class (Serializable e) => Entity e where
    update :: DeltaTime -> EntitySight -> [Broadcast] -> [Event] -> e -> (e, [Broadcast], [Action])
    getData :: e -> EntityData

data EntityWrapper = forall e. (Entity e) => EntityWrapper e