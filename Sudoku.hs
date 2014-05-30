{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

module Sudoku (
               solveOne,
               triples,
              ) where

import Data.List


-- 3 dimensional coordinate (3rd is the box!)
type Coord = (Int, Int, Int)

-- Solution value or List of remaining Candidates
data Value = Element Int | Options [Int]
           deriving (Show)

type Pair = (Coord, Value)


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

pretty :: (t, Value) -> String
pretty (_, Element e) = show e
pretty (_, Options _) = ""

compareC :: (Ord t2, Ord t3) =>
            ((t2, t3, t4), t) -> ((t2, t3, t5), t1) -> Ordering
compareC (c1, _) (c2, _) =
    compareT c1 c2
    where
      compareT (a1, b1, _) (a2, b2, _)
          | b1 == b2  = compare a1 a2
          | otherwise = compare b1 b2



readOne :: Char -> Value
readOne c =
    case c `elem` (map (head.show) ([1..9] :: [Int])) of
      True -> Element (read [c])
      False -> Options [1..9]

compute :: [Pair] -> [Pair]
compute ls =
    head $ solve done todo
    where
      (done, todo) = partition isElement ls

solve :: [Pair] -> [Pair] -> [[Pair]]
solve es [] = [es]
-- solve es ((c, Options []) : os) =
    -- error $ "no more options for " ++ show c
    -- Nothing
solve es os =
    -- map (\a -> solve ((c, Element a) : es) os') as
    case as of
      -- no more Options, no solutions possible
      [] -> []
      -- try first option
      (a : as') ->
          -- recurse using backtracking, if we can solve it
          case solve ((c, Element a) : es) os' of
            -- this branch contains no solutions, retry without it
            [] -> solve es ((c, Options as') : os')
            -- we are done
            results -> results
    where
      -- first prune all Options list at the current level, then order
      -- branches with *few* options first
      ((c, Options as) : os') = sortBy lessOptions $ map revaluate os

      lessOptions (_, Options xs) (_, Options ys) =
          compare (length xs) (length ys)
      lessOptions (_, Element _) _ =
          error "illegal lessOptions call"
      lessOptions (_, Options _) (_, Element _) =
          error "illegal lessOptions call"

      -- filter out other Options from list that are made impossible
      -- by choosing a certain one
      revaluate :: Pair -> Pair
      revaluate (c'@(x, y, z), Options aas) =
          {-# SCC "revaluate" #-}
          (c', Options aas')
          where
            aas' = aas \\ otherValues
            otherValues = map (\(_, Element e) -> e)
                          ((filter (\e -> x == px e) es) ++
                           (filter (\e -> y == py e) es) ++
                           (filter (\e -> z == pz e) es))
      revaluate ((_, _, _), Element _) =
          error "illegal revaluate call"


isElement :: (t, Value) -> Bool
isElement (_, Element _) = True
isElement (_, Options _) = False


-- project coordinate from Pair
px :: Pair -> Int
px ((x, _, _), _) = x
py :: Pair -> Int
py ((_, y, _), _) = y
pz :: Pair -> Int
pz ((_, _, z), _) = z

