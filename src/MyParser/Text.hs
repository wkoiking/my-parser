module MyParser.Text where

import qualified Data.Text as T
import Text.Parsec.Char
import Text.Parsec.Text
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Control.Monad (liftM, liftM2)
import Text.Parsec.Error (ParseError)
import Data.Char

text :: String -> Parser T.Text
text = totext . string

totext :: Parser String -> Parser T.Text
totext = liftM T.pack

ifany :: String -> Parser T.Text
ifany cs = anyOf cs <|> text ""

anyOf :: String -> Parser T.Text
anyOf = choice . map (try . text . return)

catP :: [Parser T.Text] -> Parser T.Text
catP = foldl1 (liftM2 T.append)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p fname
    = do input <- readFile fname
         return $ runP p () fname (T.pack input)

fromParser :: Parser a -> T.Text -> Bool
fromParser p = isRight . parse p ""

lexeme :: Parser a -> Parser a 
lexeme p = do { x <- p; whiteSpace; return x  }

whiteSpace :: Parser ()
whiteSpace = skipMany (simpleSpace <?> "")

simpleSpace :: Parser ()
simpleSpace = skipMany1 (satisfy isSpace)
