Sometime in autumn 2009 i created a sudoku solver in haskell as a
programming exercise for myself.

I am pretty happy with the result, it took no more than an afternoon
to implement and has been able to solve everything I tried in a couple
of seconds.
Now I can happily smile to myself whenever I see someone solving
sudokus from the newspaper ;)

github repo: https://github.com/lefant/sudoku-lefant
html rendering of the literate haskell: http://lefant.github.com/sudoku-lefant/

If you are interested in more haskell solutions to the sudoku
problem, make sure to check out [Sudoku page on haskellwiki][http://www.haskell.org/haskellwiki/Sudoku]


building and running
====================

sudoku-run
----------

ghc --make sudoku-run.hs

then to run

./sudoku-run <sudoku1.txt

sudoku-test (mini testsuite)
----------------------------

cabal install test-framework-hunit
ghc --make sudoku-test.hs

then to run

./sudoku-test
