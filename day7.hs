{-# LANGUAGE OverloadedStrings #-}
import Data.Map.Lazy (member, insert, (!), empty, Map, union, keys, assocs, fromList)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Data.Bits
import Control.Parallel
import Debug.Trace (trace)

data Operation = AND String String
  | OR String String
  | LSHIFT String Int
  | RSHIFT String Int
  | NOT String 
  | PASS String
  | Plain Int

instance Show Operation where
  show (OR x y) = x ++ " OR " ++ y
  show (AND x y) = x ++ " AND " ++ y
  show (LSHIFT x y) = x ++ " LSHIFT " ++ show y
  show (RSHIFT x y) = x ++ " RSHIFT " ++ show y
  show (NOT x) = "NOT "++x
  show (PASS x) = x
  show (Plain x) = show x

instance Eq Operation where
  (OR a b) == (OR x y) = (a,b) == (x,y)
  (AND a b) == (AND x y) = (a,b) == (x,y)
  (LSHIFT a b) == (LSHIFT x y) = (a,b) == (x,y)
  (RSHIFT a b) == (RSHIFT x y) = (a,b) == (x,y)
  (NOT a) == (NOT b) = a == b
  (PASS a) == (PASS b) = a == b
  (Plain a) == (Plain b) = a == b
  _ == _ = False
  

instance Ord Operation where
  compare (OR x y) (OR a b) = compare (x,y) (a,b)
  compare (OR _ _) _ = GT
  compare _ (OR _ _) = LT
  compare (AND x y) (AND a b) = compare (x,y) (a,b)
  compare (AND _ _) _ = GT
  compare _ (AND _ _) = LT
  compare (LSHIFT x y) (LSHIFT a b) = compare (x,y) (a,b)
  compare (LSHIFT _ _) _ = GT
  compare _ (LSHIFT _ _) = LT
  compare (RSHIFT x y) (RSHIFT a b) = compare (x,y) (a,b)
  compare (RSHIFT _ _) _ = GT
  compare _ (RSHIFT _ _) = LT
  compare (NOT a) (NOT b) = compare a b
  compare (NOT a) _ = GT
  compare _ (NOT a) = LT
  compare (PASS a) (PASS b) = compare a b
  compare (PASS a) _ = GT
  compare _ (PASS a) = LT
  compare (Plain a) (Plain b) = compare a b

variable :: Parser String
variable = choice . map try $ [many1 $ oneOf ['a'..'z'], string "1"]

number :: Parser Int
number = read <$> many1 digit

twoString :: String -> (String -> String -> Operation) -> Parser Operation
twoString st fn = do
  k1 <- variable
  char ' '
  string st
  char ' '
  k2 <- variable
  return (fn k1 k2)

stringInt :: String -> (String -> Int -> Operation) -> Parser Operation
stringInt st fn = do
  k1 <- variable
  char ' '
  string st
  char ' '
  k2 <- number
  return (fn k1 k2)

notParser :: Parser Operation
notParser = do
  string "NOT "
  v <- variable
  return (NOT v)

plainParser :: Parser Operation
plainParser = Plain <$> number

forwardParser = PASS <$> variable

parser :: Parser [(Operation, String)]
parser = many1 $ do
  op <- choice . map try $ [
        twoString "OR" OR
      , twoString "AND" AND
      , stringInt "LSHIFT" LSHIFT 
      , stringInt "RSHIFT" RSHIFT
      , notParser
      , plainParser
      , forwardParser
    ]
  string " -> "
  k <- variable
  char '\n'
  return (op, k)

getParseValue name val = either (error . show) (id) $ parse parser name val 

printPair :: (String, Operation) -> String
printPair (k, v) = show v ++ " -> " ++ k

-- fromKey :: String -> Map String Operation -> Map String In

runOp :: Operation -> Map String Operation -> Int
runOp (Plain v) _ = v
runOp (OR k1 k2) m = (arg1 `par` arg2) `pseq` (arg1 .|. arg2)
  where arg1 = (runOp (m ! k1) m)
        arg2 = (runOp (m ! k2) m)
runOp (AND k1 k2) m = (arg1 `par` arg2) `pseq` (arg1 .&. arg2)
  where arg1 = (runOp (m ! k1) m)
        arg2 = (runOp (m ! k2) m)
runOp (LSHIFT k v) m = shift (runOp (m ! k) m) v
runOp (RSHIFT k v) m = shift (runOp (m ! k) m) (-1*v)
runOp (PASS k) m = (runOp (m ! k) m)
runOp (NOT k) m = complement (runOp (m ! k) m)

main = do
  filename <- getLine
  contents <- readFile filename
  let starter = (insert "1" (Plain 1) empty) -- because they decided not to tell you that 1 is a var
  let m = foldr (\(op, key) m -> insert key op m) starter $ getParseValue filename contents
  mapM_ putStrLn . map printPair . assocs $ m
  print $ runOp (m ! "a") m
