import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
--import Test.Framework.Providers.QuickCheck (testProperty)

--import Test.QuickCheck
import Test.HUnit

import Sudoku


main = defaultMain tests

tests = [
        testGroup "sudoku result must not modify input" [
                testCase "0" test_sudoku,
                testCase "1" test_sudoku1,
                testCase "2" test_sudoku2
            ]
    ]

test_sudoku = pairlist_diff ".....8.2......693..98.7...1...........921....7......9624..9.......3..18.........3" @?= []

test_sudoku1 = pairlist_diff "123456789......................................................................." @?= []

test_sudoku2 = pairlist_diff "987654321......................................................................." @?= []



pairlist_diff string =
    (filter (uncurry (/=)) $ filter (\(a, _) -> a /= '.') $ zip string $ stuff string)
