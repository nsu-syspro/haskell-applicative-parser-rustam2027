{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

import Parser
import ParserCombinators (string, choice, char)
import Control.Applicative ((<|>))
import Data.List (elemIndex)
import Data.Maybe (fromJust)

-- | Date representation
--
-- Date parts are expected to be in following ranges
--
-- 'Day' in @[1..31]@
-- 'Month' in @[1..12]@
-- 'Year' is any non-negative integer
--
data Date = Date Day Month Year
  deriving (Show, Eq)

newtype Day   = Day   Int deriving (Show, Eq)
newtype Month = Month Int deriving (Show, Eq)
newtype Year  = Year  Int deriving (Show, Eq)

-- | Parses date in one of three formats given as BNF
--
-- @
-- date ::= dotFormat | hyphenFormat | usFormat
--
-- dotFormat ::= day "." month "." year
-- hyphenFormat ::= day "-" month "-" year
-- usFormat ::= monthName " " usDay " " year
--
-- usDate ::= nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- day ::= "0" nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- month ::= "0" nonZeroDigit | "10" | "11" | "12"
-- year ::= number
--
-- number ::= digit | number digit
-- digit ::= "0" | nonZeroDigit
-- nonZeroDigit ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
--
-- monthName ::= "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
-- @
--
-- Usage example:
--
-- >>> parse date "01.01.2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "12.12.2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "12-12-2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "Dec 12 2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 11 "")
-- >>> parse date "Jan 1 2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "Feb 31 2012"
-- Parsed (Date (Day 31) (Month 2) (Year 2012)) (Input 11 "")
-- >>> parse date "12/12/2012"
-- Failed [PosError 2 (Unexpected '/'),PosError 0 (Unexpected '1')]
--


monthNames :: [String]
monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

monthName :: Parser Month
monthName = Month . (+1) . fromJust . flip elemIndex monthNames <$> choice (string <$> monthNames)

nonZeroDigit :: Parser String
nonZeroDigit = (: []) <$> satisfy (\x ->  '1' <= x && x <= '9')

digit :: Parser String
digit = string "0" <|> nonZeroDigit

number :: Parser String
number = digit <> number <|> digit

year :: Parser Year
year = Year . read <$> number

dateEnd :: Parser String
dateEnd = choice (string <$> ["1", "2"]) <> digit <|>
  choice (string <$> ["30", "31"])

day :: Parser Day
day = Day . read <$> (
  char '0' *> nonZeroDigit <|>
  dateEnd)

usDay :: Parser Day
usDay = Day . read <$> (
  dateEnd <|>
  nonZeroDigit
  )

month :: Parser Month
month = Month . read <$>
  (char '0' *> nonZeroDigit <|>
  (string "1" <> choice (string <$> ["0", "1", "2"])))

usDate :: Parser Date
usDate = flip Date <$> monthName <* char ' ' <*> usDay <* char ' ' <*> year

hyphenFormat :: Parser Date
hyphenFormat = Date <$> day <* char '-' <*> month <* char '-' <*> year

dotFormat :: Parser Date
dotFormat = Date <$> day <* char '.' <*> month <* char '.' <*> year

date :: Parser Date
date = dotFormat <|> hyphenFormat <|> usDate
