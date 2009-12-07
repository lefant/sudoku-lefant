import Sudoku
main = do
       interact (unlines . take5 . unlines . (map stuff) . lines . concat . lines)

take5 :: [Char] -> [String]
take5 [] = []
take5 ls =
    [prefix] ++ take5 postfix
    where
      (prefix, postfix) = splitAt 9 ls
