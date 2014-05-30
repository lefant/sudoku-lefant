{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

module Sudoku (
               solveOne,
               triples,
              ) where

import Data.List
import Control.Monad

-- 3 dimensional coordinate (3rd is the box!)
type Coord = (Int, Int, Int)

data Element = Element Int deriving (Show)
data Options = Options [Int] deriving (Show)
type EPair = (Coord, Element)
type OPair = (Coord, Options)

solveOne :: String -> String
solveOne ls =
    concatMap pretty $
    sortBy compareC $
    compute $
    zip triples $
    -- map readOne $ concat $ lines ls
    map readOne ls


-- 3 dimensional coordinates with box as 3rd
triples :: [Coord]
triples =
    zip3 a b $ map z pairs
    where
      pairs = [(a', b') | b' <- [1..9], a' <- [1..9]]
      (a, b) = unzip pairs

      z :: (Int, Int) -> Int
      z (x, y) =
          x2z + y2z
          where
            x2z = ((x - 1) `div` 3) + 1
            y2z = ((y - 1) `div` 3) * 3

pretty :: (t, Element) -> String
pretty (_, Element e) = show e

compareC :: (Ord t2, Ord t3) =>
            ((t2, t3, t4), t) -> ((t2, t3, t5), t1) -> Ordering
compareC (c1, _) (c2, _) =
    compareT c1 c2
    where
      compareT (a1, b1, _) (a2, b2, _)
          | b1 == b2  = compare a1 a2
          | otherwise = compare b1 b2



readOne :: Char -> Options
readOne c =
    case c `elem` (map (head.show) ([1..9] :: [Int])) of
      True -> Options [(read [c])]
      False -> Options [1..9]

compute :: [OPair] -> [EPair]
compute ls =
    head $ foldM solve [] ls

solve :: [EPair] -> OPair -> [[EPair]]
solve es (c, Options as) =
    map (\a -> (c, Element a) : es) aas
    where
      -- filter remove Options already taken by other coordinates
      aas = as \\ otherValues
      otherValues = map (\(_, Element e) -> e)
                      ((filter (\e -> x == px e) es) ++
                       (filter (\e -> y == py e) es) ++
                       (filter (\e -> z == pz e) es))
      (x, y, z) = c


-- project coordinate from Pair
px :: EPair -> Int
px ((x, _, _), _) = x
py :: EPair -> Int
py ((_, y, _), _) = y
pz :: EPair -> Int
pz ((_, _, z), _) = z

