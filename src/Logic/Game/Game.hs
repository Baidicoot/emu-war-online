module Logic.Game.Game where

import Types.World.Block
import Types.World.World
import Types.Entity.Entity
import Types.Entity.Broadcast
import Utils.Utils

import Logic.Game.Types

import qualified Data.Map.Strict as Map

data GameState = GameState {
    blockTable :: BlockDataTable,
    loadedRegions :: RegionBlobTable,
    entities :: RegionEntityTable
}

events :: GameState -> [Event]
events _ = []

genSight :: GameState -> EntityWrapper -> EntitySight
genSight _ _ = EntitySight [] []

shouldLoad :: GameState -> RegionIndex -> Bool
shouldLoad s i = Map.member i (loadedRegions s)

load :: GameState -> RegionIndex -> Region -> GameState
load s i v = s {loadedRegions = Map.insert i v (loadedRegions s)}

doTick :: DeltaTime -> RegionMap [Broadcast] -> GameState -> (GameState, RegionMap [Broadcast], RegionMap [Action])
doTick dt bsm s =
    let sights = fmap (map $ genSight s) (entities s)
        update_ = update dt (events s)
        e' = fmap (\(ss, es, bs) ->
                let update__ = update_ bs in
                    map (\(s, e) -> update__ e s) (zip ss es))
            (zip3' (entities s) sights bsm)
        entityMap = fmap (map (\(e, _, _) -> e)) e'
        broadcasts = fmap (concatMap (\(_, b, _) -> b)) e'
        actions = fmap (concatMap (\(_, _, a) -> a)) e' in
            (s {entities = entityMap}, broadcasts, actions)

cull :: GameState -> GameState
cull s = s {entities=e'}
    where
        e' = fmap (\r -> [e | e <- r, hp (getData e) > 0]) (entities s)