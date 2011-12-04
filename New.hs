import Data.List
import Control.Monad
import Control.Arrow
import Debug.Trace

type Pos = (Integer,Integer)

data Rules
    = Rules {
        birth :: [Int]
      , survival :: [Int]
      }
    deriving (Show)

-- Determine all positions that are neigbors to the given position
neighbors :: Pos -> [Pos]
neighbors (x,y) = do [dx, dy] <- replicateM 2 [-1, 0, 1]
                     guard $ dx /= 0 || dy /= 0
                     return (x+dx, y+dy)

-- Given a list of elements, return a _sorted_ associative list of elements and how often they occured
frequencies :: Ord a => [a] -> [(a,Int)]
frequencies = map (head &&& length) . group . sort

-- Given a sorted list, eliminate duplicates
uniq :: Eq a => [a] -> [a]
uniq (x:y:xs) | x==y = uniq (x:xs)
              | otherwise = x : uniq (y:xs)
uniq xs = xs

-- Game Of Life, one step at a time :)
step :: Rules -> [Pos] -> [Pos]
step r cells = uniq $ do (loc, n) <- frequencies (neighbors =<< cells)
                         guard $ if loc `elem` cells
                                 then n `elem` survival r
                                 else n `elem` birth r
                         return loc
