conv '(' = 1
conv ')' = -1
conv _ = 0

main = do
  contents <- getLine >>= readFile
  putStrLn . show . length . takeWhile (\x -> x >= 0) . scanl (+) 0 . map conv $ contents
