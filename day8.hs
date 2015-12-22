import Control.Parallel
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Token (hexadecimal)
import Data.Char (chr)
import Data.Maybe (fromJust)
import Data.Hex (unhex)

escape :: String -> Char -> Parser (Char)
escape s c = const c <$> string s

hex :: Parser (Char)
hex = do
  char '\\'
  char 'x'
  d1 <- oneOf $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
  d2 <- oneOf $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
  return (head . fromJust . unhex $ [d1,d2])

lineParser :: Parser (String)
lineParser = many $ do
  c <- choice . map try $ [escape "\\\"" '"', escape "\\\\" '\\', hex, anyChar]
  return c

parser :: Parser (String)
parser = do
  char '"'
  line <- lineParser
  char '"'
  return line

getParseValue :: String -> String -> String
getParseValue name val = either (error . show) (id) $ parse parser name val 

getEscd :: String -> String
getEscd = getParseValue "day8"

main :: IO ()
main = do
  contents <- getLine >>= readFile
  let escd = sum . map (length . getEscd) . lines $ contents
  let norm = sum . map length . lines $ contents
  print $ (escd `par` norm) `pseq` (norm - escd)
