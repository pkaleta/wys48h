import System
import Text.ParserCombinators.Parsec hiding (spaces)
import Monad
import Char
import Numeric
import Ratio
import Complex
import Data.Array

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | String String
            | Bool Bool
            | Char Char
            | Float Double
            | Ratio Rational
            | Complex (Complex Double)
            | Vector (Array Integer LispVal)

instance Show LispVal where show = showVal

parseVector :: Parser LispVal
parseVector = do
    try $ string "#("
    values <- sepBy parseExpr spaces
    char ')'
    return $ Vector (listArray (0, fromIntegral $ length values - 1) values)

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted:: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnQuoted:: Parser LispVal
parseUnQuoted = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseList :: Parser LispVal
parseList = do
    char '('
    x <- try parseNormalList <|> parseDottedList
    char ')'
    return x

parseNormalList :: Parser LispVal
parseNormalList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseChar :: Parser LispVal
parseChar = do
    try $ string "#\\"
    text <- try (string "newline" <|> string "space") <|> do
        c <- anyChar
        return [c]
return $ Char $ case text of "space" -> ' ' "newline" -> '\n' otherwise -> text !! 0 parseFloat :: Parser LispVal parseFloat = do a <- many1 digit char '.' b <- many1 digit
    return $ Float $ extractRead (readFloat (a ++ "." ++ b))

parseRatio :: Parser LispVal
parseRatio = do
    a <- many1 digit
    char '/'
    b <- many1 digit
    return $ Ratio (read a % read b)

parseComplex :: Parser LispVal
parseComplex = do
    a <- try (parseFloat <|> parseDec)
    char '+'
    b <- try (parseFloat <|> parseDec)
    char 'i'
    return $ Complex (toDouble a :+ toDouble b)

toDouble :: LispVal -> Double
toDouble (Float f) = f
toDouble (Number n) = fromIntegral n

extractRead :: (Num a) => [(a, String)] -> a
extractRead x = fst $ x !! 0

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ noneOf "\"\\" <|> escapedChars
    char '"'
    return $ String x

escapedChars :: Parser Char
escapedChars = do
    char '\\'
    x <- oneOf "\\\"nrt"
    return $ case x of
        '\\' -> x
        '\"' -> x
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> symbol <|> digit)
    let atom = [first] ++ rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        otherwise -> Atom atom

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseBool :: Parser LispVal
parseBool = do
    try $ char '#'
    x <- oneOf "tf"
    return $ case x of
        't' -> Bool True
        'f' -> Bool False

parseNumber :: Parser LispVal
parseNumber = do
    parseNatural
    <|> try parseFloat
    <|> try parseComplex
    <|> parseRatio

parseNatural :: Parser LispVal
parseNatural = do
    parseDec
    <|> parseBin
    <|> parseOct
    <|> parseHex

parseDec :: Parser LispVal
parseDec = parseDec1 <|> parseDec2

parseBin :: Parser LispVal
parseBin = do
    try $ string "#b"
    b <- many1 $ oneOf "10"
    return $ Number (bin2dec b)

parseHex :: Parser LispVal
parseHex = do
    try $ string "#x"
    x <- many1 hexDigit
    return $ Number (hex2dec x)

parseOct :: Parser LispVal
parseOct = do
    try $ string "#x"
    x <- many1 octDigit
    return $ Number (oct2dec x)

parseDec1 :: Parser LispVal
parseDec1 = do
    try $ string "#d"
    x <- many1 digit
    return $ Number (read x)

parseDec2 :: Parser LispVal
parseDec2 = do
    x <- many1 digit
    return $ Number (read x)

bin2dec :: String -> Integer
bin2dec b = bin2dec' 0 b

bin2dec' :: Integer -> String -> Integer
bin2dec' acc "" = acc
bin2dec' acc (x:xs) = (bin2dec' $ acc * 2 + (fromIntegral $ digitToInt x)) xs

hex2dec :: String -> Integer
hex2dec x = fst $ readHex x !! 0

oct2dec :: String -> Integer
oct2dec o = fst $ readOct o !! 0

--parseNumber = liftM (Number . read) $ (many1 digit)
--parseNumber = do
--    x <- many1 digit
--    (return . Number . read) x
--parseNumber = many1 digit >>= return . Number . read

parseExpr :: Parser LispVal
parseExpr = parseString
    <|> parseNumber
    <|> parseChar
    <|> parseAtom
    <|> parseVector
    <|> parseBool
    <|> parseQuoted
    <|> parseQuasiQuoted
    <|> parseUnQuoted
    <|> parseList

spaces :: Parser ()
spaces = skipMany space

showVal :: LispVal -> String
showVal (String s) = "\"" ++ s ++ "\""
showVal (Atom a) = a
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List values) = "(" ++ unwordsList values ++ ")"
showVal (DottedList head tail) =
    "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom f : args)) = apply f $ map eval args

apply :: String -> [LispVal] -> LispVal
apply f args = maybe (Bool False) ($ args) $ lookup f primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [
     ("+", numericBinop (+))
    ,("-", numericBinop (-))
    ,("*", numericBinop (*))
    ,("/", numericBinop div)
    ,("quotient", numericBinop quot)
    ,("remainder", numericBinop rem)
    ,("symbol?", unaryOp isAtom)
    ,("string?", unaryOp isString)
    ,("number?", unaryOp isNumber)
    ,("bool?", unaryOp isBool)
    ,("list?", unaryOp isList)
    ,("symbol->string", unaryOp sym2str)
    ,("string->symbol", unaryOp str2sym)
    ]

-- binary numeric operators
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op args = Number $ foldl1 op $ map unpack args

unpack :: LispVal -> Integer
unpack (Number n) = n
unpack _ = 0

-- unary operators
unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [a] = f a

isAtom :: LispVal -> LispVal
isAtom (Atom _) = Bool True
isAtom _ = Bool False

isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _ = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _ = Bool False

isBool :: LispVal -> LispVal
isBool (Bool _) = Bool True
isBool _ = Bool False

isList :: LispVal -> LispVal
isList (List _) = Bool True
isList _ = Bool False

-- symbol management operators
sym2str :: LispVal -> LispVal
sym2str (Atom s) = String s

str2sym :: LispVal -> LispVal
str2sym (String s) = Atom s

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)
