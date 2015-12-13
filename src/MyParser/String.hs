{-# LANGUAGE FlexibleContexts #-}
module MyParser.String where

import Text.Parsec.Char
-- import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Control.Applicative (liftA2, (<*))
import Text.Parsec.Error (ParseError)
import Data.Char

ifany :: (Monad m, Stream String m Char) => String -> ParsecT String u m String
ifany cs = anyOf cs <|> string ""

anyOf ::(Monad m, Stream String m Char) =>  String -> ParsecT String u m String
anyOf = choice . map (try . string . return)

catP :: (Monad m, Stream String m Char) => [ParsecT String u m String] -> ParsecT String u m String
catP = foldl1 (liftA2 (++))

-- fromParser :: (Monad m) => ParsecT String u m a -> String -> Bool
-- fromParser p = isRight . parse p ""

lexeme :: (Monad m, Stream String m Char) => ParsecT String u m a -> ParsecT String u m a 
lexeme = lexeme' (const True)

lexemeIL :: (Monad m, Stream String m Char) => ParsecT String u m a -> ParsecT String u m a
lexemeIL = lexeme' (/= '\n')

lexeme' :: (Monad m, Stream String m Char) => (Char -> Bool) -> ParsecT String u m a -> ParsecT String u m a 
lexeme' p parser = parser <* skipMany someSimpleSpace
 where someSimpleSpace = satisfy $ liftA2 (&&) isSpace p

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

delimitor :: (Monad m, Stream String m Char) => ParsecT String u m Char
delimitor = lexeme $ char ';'


eol :: (Monad m, Stream String m Char) => ParsecT String u m Char
eol = lexeme $ char '\n'
