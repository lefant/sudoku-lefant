Lefants haskell sudoku solver
-----------------------------

There are two helper programs:

 * sudoku-test runs some (very very basic) tests.
 * sudoku-run is the binary for normal invocation, it will read from
   stdin and output to stdout. use like this:


> {-
> $ cat <<EOF | ./sudoku-run
> .98......
> ....7....
> ....15...
> 1........
> ...2....9
> ...9.6.82
> .......3.
> 5.1......
> ...4...2.
> EOF
>
> results in:
>
> 798624315
> 315879246
> 264315978
> 129587463
> 683241759
> 457936182
> 942158637
> 531762894
> 876493521
> -}


My favourite compiler flags:

> {-# OPTIONS -O2 -Wall -Werror -Wwarn #-}


This is the main module, containing the actual logic.

> module Sudoku (
>                solveOne,
>               ) where
>
> import Data.List


The Coord type is a three-dimensional coordinate, the 3rd one is the
box the field is in, like indicated here:

> {-
> +-----------+
> |111|222|333|
> |111|222|333|
> |111|222|333|
> +-----------+
> |444|555|666|
> |444|555|666|
> |444|555|666|
> +-----------+
> |777|888|999|
> |777|888|999|
> |777|888|999|
> +-----------+
> -}

> type Coord = (Int, Int, Int)


Value holds a solution value or a list of remaining valid candidates for the field.

> data Value = Element Int | Options [Int]
>            deriving (Show)

An actual field consists of a coordinate and a Value (as described above).

> type Pair = (Coord, Value)



This is the main exported function. It will read in a string of digits
or . and feed it to the solve' function which will find a solution
using solve and then return a prettified string representation.

> solveOne :: String -> String
> solveOne ls =
>     concatMap pretty $
>     sortBy compareC $
>     solve' $
>     zip triples $
>     map readOne ls


This will return a list of three-dimensional coordinates as explained
with the Coord type above.

> triples :: [Coord]
> triples =
>     zip3 a b $ map z pairs
>     where
>       pairs = [(a', b') | b' <- [1..9], a' <- [1..9]]
>       (a, b) = unzip pairs
>
>       z :: (Int, Int) -> Int
>       z (x, y) =
>           x2z + y2z
>           where
>             x2z = ((x - 1) `div` 3) + 1
>             y2z = ((y - 1) `div` 3) * 3


> pretty :: (t, Value) -> String
> pretty (_, Element e) = show e
> pretty (_, Options _) = ""


Used for sorting coordinates from left to right and top to bottom.

> compareC :: (Ord t2, Ord t3) =>
>             ((t2, t3, t4), t) -> ((t2, t3, t5), t1) -> Ordering
> compareC (c1, _) (c2, _) =
>     compareT c1 c2
>     where
>       compareT (a1, b1, _) (a2, b2, _)
>           | b1 == b2  = compare a1 a2
>           | otherwise = compare b1 b2



Read in a predefined single value or failing that initialize the list
of options.

> readOne :: Char -> Value
> readOne c =
>     case c `elem` (map (head.show) ([1..9] :: [Int])) of
>       True -> Element (read [c])
>       False -> Options [1..9]



solve' and solve contain the actual solving logic. solve' will
partition the initial list of fields into ones containing single
elements (already defined / solved) and those containing a list of
remaining options.

> solve' :: [Pair] -> [Pair]
> solve' ls =
>     solution
>     where
>       Just solution = solve done todo
>       (done, todo) = partition isElement ls
>       isElement :: (t, Value) -> Bool
>       isElement (_, Element _) = True
>       isElement (_, Options _) = False


solve takes two lists of coordinate / value pairs as parameters: the
first one contains solved single element fields, the second all the
lists with remaining options.

> solve :: [Pair] -> [Pair] -> Maybe [Pair]

if all the fields have one element we are done.

> solve es [] = Just es
> solve es os =
>     case as of
>       -- no more Options, no solutions possible
>       [] -> Nothing
>       -- try first option
>       (a : as') ->
>           -- recurse using backtracking, if we can solve it
>           case solve ((c, Element a) : es) os' of
>             -- we are done
>             Just es' ->
>                 Just es'
>             -- this branch contains no solutions, retry without it
>             Nothing ->
>                 solve es ((c, Options as') : os')
>
>     where
>       -- first prune all Options list at the current level, then order
>       -- branches with *few* options first
>       ((c, Options as) : os') = sortBy lessOptions $ map revaluate os
>
>       lessOptions (_, Options xs) (_, Options ys) =
>           compare (length xs) (length ys)
>       lessOptions (_, Element _) _ =
>           error "illegal lessOptions call"
>       lessOptions (_, Options _) (_, Element _) =
>           error "illegal lessOptions call"
>
>       -- filter out other Options from list that are made impossible
>       -- by choosing a certain one
>       revaluate :: Pair -> Pair
>       revaluate (c'@(x, y, z), Options aas) =
>           {-# SCC "revaluate" #-}
>           (c', Options aas')
>           where
>             aas' = aas \\ otherValues
>             otherValues = map (\(_, Element e) -> e)
>                           ((filter (\e -> x == px e) es) ++
>                            (filter (\e -> y == py e) es) ++
>                            (filter (\e -> z == pz e) es))
>       revaluate ((_, _, _), Element _) =
>           error "illegal revaluate call"


helper functions to project a single coordinate from a Pair

> px :: Pair -> Int
> px ((x, _, _), _) = x
> py :: Pair -> Int
> py ((_, y, _), _) = y
> pz :: Pair -> Int
> pz ((_, _, z), _) = z
