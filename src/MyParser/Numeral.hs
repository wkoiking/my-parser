module MyParser.Numeral where

import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Data.Char
import Numeric hiding (showHex)
import MyParser.String

float :: Monad m => ParsecT String u m Double
float = lexemeIL floating <?> "float"

integer :: Monad m => ParsecT String u m Integer
integer = lexemeIL int <?> "integer"

natural :: Monad m => ParsecT String u m Integer
natural = lexemeIL nat <?> "natural"

decimal :: Monad m => ParsecT String u m Integer
decimal = signedNumber 10 digit <?> "decimal"

hexadecimal :: Monad m => ParsecT String u m Integer
hexadecimal = signedNumber 16 hexDigit <?> "hexadecimal"

octal :: Monad m => ParsecT String u m Integer
octal = signedNumber 8 octDigit <?> "octal"

binary :: Monad m => ParsecT String u m Integer
binary = signedNumber 2 binDigit <?> "binary"

number base baseDigit = do
  digits <- many1 baseDigit
  let n = foldl (\x d -> base * x + toInteger (digitToInt d)) 0 digits
  seq n (return n)

signed :: Monad m => ParsecT String u m Integer -> ParsecT String u m Integer
signed lexemeILNum = do
  f <- lexemeIL sign
  n <- lexemeILNum
  return (f n)

signedNumber :: Monad m => Integer -> ParsecT String u m Char -> ParsecT String u m Integer
signedNumber baseNum digitCharP = lexemeIL $ do
  f <- lexemeIL sign
  n <- number baseNum digitCharP
  return (f n)

naturalOrFloat :: Monad m => ParsecT String u m (Either Integer Double)
naturalOrFloat  = lexemeIL (natFloat) <?> "number"

binDigit :: Monad m => ParsecT String u m Char
binDigit = satisfy isBinDigit

isBinDigit :: Char -> Bool
isBinDigit x = '0' <= x && x <= '1'

showHex :: (Integral a,Show a) => a -> String
showHex x = showIntAtBase 16 intToDigit x ""

showOct :: (Integral a,Show a) => a -> String
showOct x = showIntAtBase 8 intToDigit x ""

showBin :: (Integral a,Show a) => a -> String
showBin x = showIntAtBase 2 intToDigit x ""

-- floats
floating :: Monad m => ParsecT String u m Double
floating = do
  n <- decimal
  try (fractExponent n) <|> return (fromInteger n)

natFloat :: Monad m => ParsecT String u m (Either Integer Double)
natFloat = do { char '0' ; zeroNumFloat } <|> decimalFloat

zeroNumFloat :: Monad m => ParsecT String u m (Either Integer Double)
zeroNumFloat = do { n <- hexadecimal <|> octal ; return (Left n) }
  <|> decimalFloat
  <|> fractFloat 0
  <|> return (Left 0)

decimalFloat :: Monad m => ParsecT String u m (Either Integer Double)
decimalFloat
  = do{ n <- decimal ; option (Left n) (fractFloat n) }

fractFloat :: Monad m => Integer -> ParsecT String u m (Either a Double)
fractFloat n
  = do{ f <- fractExponent n ; return (Right f) }

fractExponent :: Monad m => Integer -> ParsecT String u m Double
fractExponent n
  = do { fract <- fraction ; expo  <- option 1.0 exponent' ; return ((fromInteger n + fract) * expo) }
    <|> do { expo <- exponent' ; return ((fromInteger n)*expo) }

-- .53
fraction :: Monad m => ParsecT String u m Double
fraction
  = do{ char '.' ; digits <- many1 digit <?> "fraction" ; return (foldr op 0.0 digits) } <?> "fraction"
 where op d f = (f + fromIntegral (digitToInt d))/10.0

-- e5, e-5
exponent' :: Monad m => ParsecT String u m Double
exponent'
  = do { oneOf "eE" ; f <- sign ; e <- decimal <?> "exponent" ; return (power (f e)) } <?> "exponent"
 where power e  | e < 0      = 1.0 / power(- e)
                | otherwise  = fromInteger (10^e)


-- integers and naturals
int :: Monad m => ParsecT String u m Integer
int = do{ f <- lexemeIL sign ; n <- nat ; return (f n) }

sign :: (Monad m, Num a) => ParsecT String u m (a -> a)
sign = (char '-' >> return negate) <|> (char '+' >> return id) <|> return id

nat :: Monad m => ParsecT String u m Integer
nat = {- zeroNumber <|> -} decimal -- intのバグフィックス referenceとこちらのhexadecimal, octal, decimalは挙動が違う。

zeroNumber :: Monad m => ParsecT String u m Integer
zeroNumber = do { char '0' ; hexadecimal <|> octal <|> decimal <|> return 0 } <?> ""

