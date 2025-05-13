{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

import Parser
import ParserCombinators
import Data.Char (isDigit, digitToInt)
import Control.Applicative (Alternative(some, (<|>)))

getIntegerFromString :: [Char] -> Integer
getIntegerFromString = toInteger . go 0 where
  go s (x:_) = s * 10 + digitToInt x
  go s [] = s

-- | Parses natural number (including zero)
--
-- Usage example:
--
-- >>> parse nat "0"
-- Parsed 0 (Input 1 "")
-- >>> parse nat "123"
-- Parsed 123 (Input 3 "")
-- >>> parse nat "-123"
-- Failed [PosError 0 (Unexpected '-')]
-- >>> parse nat "abc"
-- Failed [PosError 0 (Unexpected 'a')]
-- >>> parse nat "123abc"
-- Parsed 123 (Input 3 "abc")
--
nat :: Parser Integer
nat = read <$> some (satisfy isDigit)

neg :: Parser Integer
neg = char '-' *> ((*) (-1) . read <$> some (satisfy isDigit))

-- | Parses integer number
--
-- Usage example:
--
-- >>> parse int "0"
-- Parsed 0 (Input 1 "")
-- >>> parse int "123"
-- Parsed 123 (Input 3 "")
-- >>> parse int "-123"
-- Parsed (-123) (Input 4 "")
-- >>> parse int "abc"
-- Failed [PosError 0 (Unexpected 'a')]
-- >>> parse int "123abc"
-- Parsed 123 (Input 3 "abc")
--
int :: Parser Integer
int = neg <|> nat

