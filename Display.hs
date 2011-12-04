module Display where

import           Core
import qualified Data.Set  as S
import qualified Data.Map  as M
import           Data.List

extents :: World -> (Coord, Coord)
extents w = (Coord (minimum xs) (minimum ys),
             Coord (maximum xs) (maximum ys))
    where (xs, ys) = (unzip . map (\(Coord x y) -> (x, y)) . S.toList) w

groupX :: World -> M.Map Int [Int]
groupX = M.fromAscListWith (++) . map (\(Coord x y) -> (x, [y])) . S.toAscList

minmax :: [Int] -> (Int, Int)
minmax [] = (0, 0)
minmax xs = (minimum xs, maximum xs)

