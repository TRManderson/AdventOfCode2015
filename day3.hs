import Data.List (nub)

mv :: Char -> (Int, Int) -> (Int, Int)
mv c (x,y)
  | c == '>' = (x+1, y)
  | c == '<' = (x-1, y)
  | c == '^' = (x, y+1)
  | c == 'v' = (x, y-1)

main = do
  filename <- getLine
  contents <- readFile filename
  print . length . nub . scanr ($) (0,0) . map mv $ contents
