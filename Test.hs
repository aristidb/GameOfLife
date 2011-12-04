module Test where

import Core
import qualified Data.Set as S
import Data.List

makeWorld :: [(Int,Int)] -> World
makeWorld = S.fromList . map (\(x, y) -> Coord x y)

printCoord :: Coord -> String
printCoord (Coord x y) = show x ++ "/" ++ show y

printWorld :: World -> String
printWorld = intercalate ", " . map printCoord . S.toList

printGame :: [World] -> IO ()
printGame = mapM_ (putStrLn . printWorld)

test :: [(Int,Int)] -> IO ()
test = printGame . run . makeWorld
