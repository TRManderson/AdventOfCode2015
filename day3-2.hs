import Data.List (nub)

mv :: Char -> (Int, Int) -> (Int, Int)
mv c (x,y)
  | c == '>' = (x+1, y)
  | c == '<' = (x-1, y)
  | c == '^' = (x, y+1)
  | c == 'v' = (x, y-1)

lists :: [a] -> [(a,a)]
lists (x:y:xs) = (x,y):(lists xs)
lists [] = []

houses :: [Char] -> [(Int, Int)]
houses = scanl (flip ($)) (0,0) . map mv

main = do
  filename <- getLine
  contents <- readFile filename
  let ls = unzip (lists contents)
  print . length . nub $ (houses . fst $ ls) ++ (houses . snd $ ls)