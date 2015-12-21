{-# LANGUAGE OverloadedStrings #-}
import Data.Map.Lazy (member, insert, (!), empty, Map, union, keys, lookup)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Data.Bits
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



-- fromKey :: String -> Map String Operation -> Map String In

runOp :: Operation -> Map String Operation -> Int
runOp (Plain v) _ = v
runOp (OR k1 k2) m = (runOp (m ! k1) m) .|. (runOp (m ! k2) m)
runOp (AND k1 k2) m = (runOp (m ! k1) m) .&. (runOp (m ! k2) m)
runOp (LSHIFT k v) m = shift (runOp (m ! k) m) v
runOp (RSHIFT k v) m = shift (runOp (m ! k) m) (-1*v)
runOp (PASS k) m = (runOp (m ! k) m)
runOp (NOT k) m = complement (runOp (m ! k) m)

main = do
  filename <- getLine
  contents <- readFile filename
  let starter = (insert "1" (Plain 1) empty) -- because they decided not to tell you that 1 is a var
  let m = foldr (\(op, key) m -> insert key op m) starter $ getParseValue filename contents
  

  print . keys $ m
  print $ runOp (m ! "a") m
