
module StreetNumber where

import           Control.Applicative
import           Data.ByteString       (ByteString)
import           Data.Char
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Text.Encoding
import           Test.Hspec
import           Text.Parser.LookAhead
import           Text.RawString.QQ
import           Text.Trifecta

import           Components

newtype Suffix = Suffix Text deriving (Show, Eq, Ord)
newtype Prefix = Prefix Text deriving (Show, Eq, Ord)
newtype Number = Number Text deriving (Show, Eq, Ord)

data Single = Single Prefix Number Suffix deriving (Show, Eq, Ord)

data StreetNumber = One Single
                  | Range Single Single
                  deriving (Show, Eq, Ord)

singleNumber :: Parser Text
singleNumber = some digit >>= \digits ->
  if (head digits == '0') || (length digits > 5)
  then fail $ "Too many digits: " <> digits
  else pure (T.pack digits)

singleFix :: Parser Text
singleFix = many letter >>= \letters ->
  if length letters > 2
  then fail $ "Too many letters: " <> letters
  else pure (T.pack letters)

single :: Parser Single
single = do
  p <- singleFix
  n <- singleNumber
  s <- singleFix
  pure $ Single (Prefix p) (Number n) (Suffix s)

oneNumber :: Parser StreetNumber
oneNumber = do
  n <- single
  _ <- lookAhead spaceOrComma' <* (notFollowedBy $ char '-')
  pure $ One n

rangeNumber :: Parser StreetNumber
rangeNumber = do
  startN <- single
  _ <- spaces
  _ <- char '-'
  _ <- spaces
  endN <- single
  _ <- lookAhead spaceOrComma' <* (notFollowedBy $ char '-')
  pure $ Range startN endN

streetNumber :: Parser StreetNumber
streetNumber = try rangeNumber <|> oneNumber
