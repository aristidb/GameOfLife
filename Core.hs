module Core where

import Data.Set (Set, (\\))
import qualified Data.Set as S


unionMap :: (Ord a, Ord b) => (a -> Set b) -> Set a -> Set b
unionMap f = S.fromList . concatMap (S.toList . f) . S.toList


data Coord = Coord { cX :: Int, cY :: Int } deriving (Eq, Ord, Show)

data Delta = Delta { dX :: Int, dY :: Int } deriving (Eq, Ord, Show)

(.+.) :: Coord -> Delta -> Coord
Coord x y .+. Delta dx dy = Coord (x + dx) (y + dy)

type World = Set Coord


dNeighborsOrSelf :: Set Delta
dNeighborsOrSelf =  S.fromList $ [Delta x y | x <- i, y <- i]
    where i = [-1, 0, 1]

dNeighbors :: Set Delta
dNeighbors = S.delete (Delta 0 0) dNeighborsOrSelf

neighborsOf :: Coord -> World
neighborsOf c = S.mapMonotonic (c .+.) dNeighbors


liveNeighbors :: World -> Coord -> World
liveNeighbors w c = neighborsOf c `S.intersection` w


data LiveOrDie = Die | Survive | Respawn deriving (Eq, Ord, Show)

respawn :: LiveOrDie -> Bool
respawn = (== Respawn)

survive :: LiveOrDie -> Bool
survive = (/= Die)

liveOrDie :: World -> Coord -> LiveOrDie
liveOrDie w c = case S.size (liveNeighbors w c) of
                  2 -> Survive
                  3 -> Respawn
                  _ -> Die


survivers :: World -> World
survivers world = S.filter (survive . liveOrDie world) world

respawners :: World -> World
respawners world = S.filter (respawn . liveOrDie world) worldNeighbors
    where worldNeighbors = unionMap neighborsOf world


nextGeneration :: World -> World
nextGeneration world = survivers world `S.union` respawners world

run :: World -> [World]
run w = go w S.empty
    where go w o | w == o = []
                 | otherwise = w : go (nextGeneration w) w
