#! /usr/bin/env stack
-- stack runghc
import Data.Array.MArray
import Data.Ix (range)
import Data.Array.IO (IOArray)
import Control.Monad (liftM)
import Text.Parsec 
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, string, digit)

countLit :: IOArray (Int, Int) Bool -> IO Int
countLit = liftM (length . filter id) . getElems

toggle :: (Ix i) => i -> i -> IOArray i Bool -> IO ()
toggle x y a = mapM_ (($ True) . writeArray a) $ range (x, y)

turnOn :: (Ix i) => i -> i -> IOArray i Bool -> IO ()
turnOn x y a = mapM_ (($ True) . writeArray a) $ range (x, y)

turnOff :: (Ix i) => i -> i -> IOArray i Bool -> IO ()
turnOff x y a = mapM_ (($ False) . writeArray a) $ range (x, y)

getFn :: (Ix i) => String -> i -> i -> IOArray i Bool -> IO ()
getFn xs
  | xs == "turn on" = turnOn
  | xs == "turn off" = turnOff
  | xs == "toggle" = toggle
  | otherwise = undefined

tuplify :: [a] -> (a,a)
tuplify [x,y] = (x,y)

how2parse :: Parser (IOArray (Int,Int) Bool -> IO ())
how2parse = do
    fnName <- choice [string "turn off", string "turn on", string "toggle"]
    char ' '
    pair1 <- sepBy1 ((read :: String -> Int) <$> many1 digit) (char ',')
    string " through "
    pair2 <- sepBy1 ((read :: String -> Int) <$> many1 digit) (char ',')
    return $ (getFn fnName) (tuplify pair1) (tuplify pair2)

modifier :: String -> IOArray (Int, Int) Bool -> IO ()
modifier s = either (error . show) id . (parse how2parse s) $ s

main :: IO ()
main = do
  contents <- readFile =<< getLine
  a <- newArray ((0, 0), (999, 999)) False
  mapM_ (($ a) . modifier) . lines $ contents
  countLit a >>= print
