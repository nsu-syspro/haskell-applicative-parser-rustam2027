{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- The above pragma temporarily disables warnings about Parser constructor and runParser not being used

module Parser
  ( -- * Important note
    -- 
    -- | The implementation of 'Parser' is intentionally
    -- hidden to other modules to encourage use of high level
    -- combinators like 'satisfy' and the ones from 'ParserCombinators'
    Parser
  , parse
  , parseMaybe
  , satisfy
  , Error(..)
  , Position(..)
  , Parsed(..)
  , Input
  , option
  ) where

import Control.Applicative
import Data.List (nub)

-- | Value annotated with position of parsed input starting from 0
data Position a = Position Int a
 deriving (Show, Eq)

-- | Parser input encapsulating remaining string to be parsed with current position
type Input = Position String

-- | Parsing error
data Error =
    Unexpected Char -- ^ Unexpected character
  | EndOfInput      -- ^ Unexpected end of input
 deriving (Show, Eq)

-- | Parsing result of value of type @a@
data Parsed a =
    Parsed a Input           -- ^ Successfully parsed value of type @a@ with remaining input to be parsed
  | Failed [Position Error]  -- ^ Failed to parse value of type @a@ with accumulated list of errors
 deriving Show

-- | Parser of value of type @a@
newtype Parser a = Parser { runParser :: Input -> Parsed a }

-- | Runs given 'Parser' on given input string
parse :: Parser a -> String -> Parsed a
parse (Parser run) = run . Position 0

-- | Runs given 'Parser' on given input string with erasure of @Parsed a@ to @Maybe a@
parseMaybe :: Parser a -> String -> Maybe a
parseMaybe p s = case parse p s of
  Parsed result _ -> Just result
  Failed _        -> Nothing

instance Functor Parsed where
  fmap _ (Failed err) = Failed err
  fmap f (Parsed a inp) = Parsed (f a) inp

instance Applicative Parsed where
  pure = undefined

  (<*>) (Parsed f _) (Parsed a inp2) = Parsed (f a) inp2
  (<*>) (Failed e1) (Failed e2) = Failed (nub (e1 ++ e2))
  (<*>) (Failed e) _ = Failed e
  (<*>) _ (Failed e) = Failed e

instance Alternative Parsed where
  empty = Failed []
  r@(Parsed _ _) <|> _ = r
  (Failed _) <|> r@(Parsed _ _) = r
  (Failed err1) <|> (Failed err2) = Failed (nub (err1 ++ err2))

instance Functor Parser where
  fmap f (Parser run) = Parser $ \x -> fmap f (run x)

instance Applicative Parser where
  pure a = Parser $ \x -> Parsed a x
  (<*>) (Parser fab) (Parser runA) = Parser $ \x -> case fab x of
    Parsed f inp0 -> case runA inp0 of
      Parsed a inp1 -> Parsed (f a) inp1
      Failed err    -> Failed err
    Failed err    ->  Failed err

instance Semigroup a => Semigroup (Parser a) where
  (<>) (Parser runA1) (Parser runA2) = Parser $ \x -> case runA1 x of
    Failed err -> Failed err
    Parsed a1 int1 -> case runA2 int1 of
      Failed err -> Failed err
      Parsed a2 int2 -> Parsed (a1 <> a2) int2

instance Alternative Parser where
  empty = Parser $ const empty
  -- Note: when both parsers fail, their errors are accumulated and *deduplicated* to simplify debugging
  (<|>) (Parser pa) (Parser pb) = Parser $ \x -> pa x <|> pb x

option :: Monoid m => Parser m -> Parser m
option (Parser p) = Parser $ \x -> case p x of
  Failed _ -> Parsed mempty x
  r -> r

-- | Parses single character satisfying given predicate
--
-- Usage example:
--
-- >>> parse (satisfy (>= 'b')) "foo"
-- Parsed 'f' (Position 1 "oo")
-- >>> parse (satisfy (>= 'b')) "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (satisfy (>= 'b')) "abc"
-- Failed [Position 0 (Unexpected 'a')]
-- >>> parse (satisfy (>= 'b')) ""
-- Failed [Position 0 EndOfInput]
--
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \(Position i str) -> case str of
  []  -> Failed [Position i EndOfInput]
  (c:_) -> if p c then Parsed c (Position (i + 1) (drop 1 str)) else Failed [Position i (Unexpected c)]
  
