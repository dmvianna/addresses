
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
  if length digits > 5
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

-- instance Eq a => Eq (Result a) where
--   Success x == Success y = x == y
--   _ == _ = False


-- main :: IO ()
-- main = hspec $ do

--   describe "street number" $ do
--     it "parses single street number" $ do
--       let actual = parseByteString streetNumber mempty "12B "
--           expected = One $ Single (Prefix "") (Number "12") (Suffix "B")
--       actual `shouldBe` Success expected
--     it "parses range number" $ do
--       let actual = parseByteString streetNumber mempty "A12B-A24C "
--           expected = Range
--                      (Single (Prefix "A") (Number "12") (Suffix "B"))
--                      (Single (Prefix "A") (Number "24") (Suffix "C"))
--       actual `shouldBe` Success expected
--     it "fails on malformed single street number" $ do
--       case parseByteString streetNumber mempty "12B4 " of
--         Failure (ErrInfo _ actual) -> show actual `shouldBe` "[Columns 3 3]"
--         _                          -> fail "this test should fail"
--     it "fails on malformed range street number" $ do
--       case parseByteString streetNumber mempty "12B-C " of
--         Failure (ErrInfo _ actual) -> show actual `shouldBe` "[Columns 3 3]"
--         _                          -> fail "this test should fail"
