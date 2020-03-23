module Logic.Game.Game where

import Types.World.Block
import Types.World.World
import Types.Entity.Entity
import Types.Entity.Broadcast
import Utils.Utils

import Types.Logic.Game

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

migrants :: GameState -> (RegionEntityTable, RegionEntityTable)
migrants s = (e', m)
    where
        e' = Map.mapWithKey (\k es -> filter ((==) k . regionIndex . pos . getData) es) (entities s)
        n = Map.mapWithKey (\k es -> filter ((/=) k . regionIndex . pos . getData) es) (entities s)
        m = (Map.fromList . map (\(k, a) -> (k, concat a)) . groupKeyList . Map.toList) n

migrate :: GameState -> GameState
migrate s = s {entities = m `Map.union` n}
    where
        (m, n) = migrants s