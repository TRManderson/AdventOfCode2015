import Data.List.Split (splitOn)

boxCalc :: [Integer] -> Integer
boxCalc (x:y:z:[]) = x*y*2 + x*z*2 + y*z*2 + extraTerm
  where extraTerm = minimum [x*y, y*z, z*x]

ribbonCalc :: [Integer] -> Integer
ribbonCalc (x:y:z:[]) = minimum [2*x+2*y, 2*y+2*z, 2*z+2*x] + x*y*z

main = do
  contents <- getLine >>= readFile
  let boxLines = map (map read . splitOn "x" :: String -> [Integer]) . lines $ contents
  putStrLn . show . sum . map ribbonCalc $ boxLines
