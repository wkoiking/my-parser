module MyParser (
  module MyParser.String, 
  module MyParser.Numeral, 
--   module Text.Parsec.Char, 
--   module Text.Parsec.String, 
--   module Text.Parsec.Combinator,
--   module Text.Parsec.Prim,
--   fileP,
--   eitherP
  ) where

import MyParser.String
import MyParser.Numeral
import Text.Parsec.Char
-- import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Control.Monad

-- fileP :: Parser a -> Parser a
-- fileP p = do
--  spaces
--  contents <- p
--  spaces
--  eof
--  return contents

-- eitherP :: Parser a -> Parser b -> Parser (Either a b)
-- eitherP leftP rightP = choice [try $ liftM Left leftP, liftM Right rightP]
