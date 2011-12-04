{-# LANGUAGE RecordWildCards #-}
import Data.List
import Control.Monad
import Control.Arrow
import Debug.Trace

type Pos = [Int]

data Rules
    = Rules {
        dimension :: Int
      , birth :: [Int]
      , survival :: [Int]
      }
    deriving (Show)

-- Determine all positions that are neigbors to the given position
neighbors :: Int -> Pos -> [Pos]
neighbors dim xs = do ds <- replicateM dim [-1, 0, 1]
                      guard $ any (/=0) ds
                      return $ zipWith (+) xs ds

-- Given a list of elements, return a _sorted_ associative list of elements and how often they occured
frequencies :: Ord a => [a] -> [(a,Int)]
frequencies = map (head &&& length) . group . sort

-- Game Of Life, one step at a time :)
step :: Rules -> [Pos] -> [Pos]
step Rules{..} cells = do (loc, n) <- frequencies (neighbors dimension =<< cells)
                          guard $ if loc `elem` cells
                                  then n `elem` survival
                                  else n `elem` birth
                          return loc
