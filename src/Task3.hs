{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

import Parser
import Data.Char ( toLower )
import Data.List (intercalate)
import ParserCombinators (char, sepBy, choice, string, count)
import Control.Applicative (Alternative(..))
import GHC.Unicode (isHexDigit)

-- | JSON representation
--
-- See <https://www.json.org>
--
data JValue =
    JObject [(String, JValue)]
  | JArray [JValue]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
 deriving (Show, Eq)

-- | Parses JSON value
--
-- See full grammar at <https://www.json.org>
--
-- Usage example:
--
-- >>> parse json "{}"
-- Parsed (JObject []) (Input 2 "")
-- >>> parse json "null"
-- Parsed JNull (Input 4 "")
-- >>> parse json "true"
-- Parsed (JBool True) (Input 4 "")
-- >>> parse json "3.14"
-- Parsed (JNumber 3.14) (Input 4 "")
-- >>> parse json "{{}}"
-- Failed [PosError 0 (Unexpected '{'),PosError 1 (Unexpected '{')]
--

object :: Parser JValue
object = JObject [] <$ (char '{' *> whiteSpace <* char '}')
     <|> JObject <$> (char '{' *> whiteSpace *> sepBy pair (char ',' <* whiteSpace) <* char '}')
  where
    pair = (,) <$> someString <* whiteSpace <* char ':' <* whiteSpace <*> value

array :: Parser JValue
array = JArray [] <$ (char '[' *> whiteSpace <* char ']')
  <|> JArray <$> (char '[' *> sepBy value (char ',') <* char ']')

whiteSpace :: Parser String
whiteSpace = many (choice (char <$> [' ', '\n', '\r', '\t']))

value :: Parser JValue
-- value = whiteSpace *> choice [
--     jstring,
--     number,
--     object,
--     array,
--     bool,
--     Task3.null
--   ] <* whiteSpace
--
value = whiteSpace *> (object <|> array <|> jstring <|> number <|> bool <|> Task3.null) <* whiteSpace

jstring :: Parser JValue
jstring = JString <$> someString

hex :: Parser Char
hex = satisfy isHexDigit

someString :: Parser String
someString = char '"' *> stringContent <* char '"' where
  stringContent :: Parser String
  stringContent = concat <$> many (((: []) <$> satisfy (\x -> x /= '"' && x /= '\\')) <|> escapeChar)
  escapeChar = string "\\" <> (choice (string <$> ["\"", "\\", ['/'], "b", "f", "n", "r", "t"]) <|> count 4 hex)

number :: Parser JValue
number = JNumber . read <$> (option (string "-")
                         <> (zero <|> (((: []) <$> digitNoZero) <> many digit)))
                         <> option (string "." <> some digit)
                         <> option (choice [string "E", string "e"] <> option (choice [string "-", string "+"]) <> some digit)

digitNoZero :: Parser Char
digitNoZero = satisfy (\x -> '0' < x && x <= '9')

zero :: Parser String
zero = string "0"

digit :: Parser Char
digit = digitNoZero <|> char '0'

bool :: Parser JValue
bool = choice [JBool True <$ string "true",
   JBool False <$ string "false"]

null :: Parser JValue
null = JNull <$ string "null"

json :: Parser JValue
json = value

-- * Rendering helpers

-- | Renders given JSON value as oneline string
render :: JValue -> String
render = concatMap readable . renderTokens
  where
    -- Adds some nice spacing for readability
    readable ":" = ": "
    readable "," = ", "
    readable s   = s

-- | Renders given JSON value as list of separate tokens ready for pretty printing
renderTokens :: JValue -> [String]
renderTokens JNull        = ["null"]
renderTokens (JBool b)    = [map toLower $ show b]
renderTokens (JNumber d)  = [show d]
renderTokens (JString s)  = ["\"" ++ s ++ "\""]
renderTokens (JArray xs)  = ["["] ++ intercalate [","] (map renderTokens xs) ++ ["]"]
renderTokens (JObject xs) = ["{"] ++ intercalate [","] (map renderPair xs) ++ ["}"]
 where
  renderPair :: (String, JValue) -> [String]
  renderPair (k, v) = ["\"" ++ k ++ "\""] ++ [":"] ++ renderTokens v

-- | Renders 'Parsed' or 'Failed' value as string
renderParsed :: Parsed JValue -> String
renderParsed (Parsed v _) = render v
renderParsed (Failed err) = show err

-- | Parses given file as JSON and renders result
renderJSONFile :: String -> IO String
renderJSONFile file = renderParsed <$> parseJSONFile file

-- | Parses given file as JSON
parseJSONFile :: String -> IO (Parsed JValue)
parseJSONFile file = parse json <$> readFile file
