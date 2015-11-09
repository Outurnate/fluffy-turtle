module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.Char
import Numeric
import Control.Monad

-- TODO:
-- Full # support
-- quasiquote/unquote
-- vectors
-- left-factor try out of list

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Integer Integer
             | String String
             | Character Char
             | Bool Bool
             | Real Double

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- (many (noneOf "\"")
       <|> (string "\\\"")
       <|> (string "\\n")
       <|> (string "\\r")
       <|> (string "\\t")
       <|> (string "\\\\"))
  char '"'
  return $ String x

parseCharacter :: Parser LispVal
parseCharacter = do
  string "#\\"
  c <- many (letter <|> digit <|> symbol)
  return $ case (length c) of
    1 -> (Character . head) c
    _ -> case (map toLower c) of
           "space"   -> Character ' '
           "newline" -> Character '\n'
           _         -> Character 'a' -- TODO

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
  str <- many1 digit
  return $ (Integer . read) str

parseNumberDecimal :: Parser LispVal
parseNumberDecimal = do
  left <- many1 digit
  char '.'
  right <- many1 digit
  return $ (Real . fst . head . readFloat) (left ++ "." ++ right)

parseNumberRadix :: Parser LispVal
parseNumberRadix = do
  char '#'
  radix <- letter
  str <- many1 (oneOf "0123456789ABCDEFabcdef")
  return $ (Integer . (case radix of
                         'b' -> fst . head . readInt 2 (`elem` "01") digitToInt
                         'd' -> fst . head . readDec
                         'o' -> fst . head . readOct
                         'x' -> fst . head . readHex)) str

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  headL <- endBy parseExpr spaces
  tailL <- char '.' >> spaces >> parseExpr
  return $ DottedList headL tailL

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseNumberFinal :: Parser LispVal
parseNumberFinal = try parseNumberRadix <|> try parseNumberDecimal <|> parseNumber

parseExpr :: Parser LispVal
parseExpr = do
              x <- try parseNumberFinal <|> parseAtom
              return x
            <|> parseCharacter
            <|> parseString
            <|> parseQuoted
            <|> do char '('
                   x <- try parseList <|> parseDottedList
                   char ')'
                   return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Integer contents) = show contents
showVal (Real contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList headL tailL) = "(" ++ unwordsList headL ++ " . " ++ showVal tailL ++ ")"

unpackNum :: LispVal -> Integer
unpackNum (Integer n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
  if null parsed
     then 0
     else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Integer $ foldl1 op $ map unpackNum params

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+",         numericBinop (+)),
              ("-",         numericBinop (-)),
              ("*",         numericBinop (*)),
              ("/",         numericBinop div),
              ("mod",       numericBinop mod),
              ("quotient",  numericBinop quot),
              ("remainder", numericBinop rem)]

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Integer _) = val
eval val@(Real _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

instance Show LispVal where show = showVal

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
