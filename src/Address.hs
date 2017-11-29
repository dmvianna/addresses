
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Address where

import           Control.Applicative
import           Data.ByteString       (ByteString)
import           Data.Char
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Text.Encoding
import           Text.Parser.LookAhead
import           Text.RawString.QQ
import           Text.Trifecta
import           Test.Hspec

import Components hiding (main)

{-|
Addresses are not standardised. Here is a non comprehensive list
of simple cases to look for:

1. 12 The Horizon, Epping VIC 3076 -- We can consider 'The' as a prefix street type.
2. 12 Elizabeth Street, Malvern VIC 3144 -- 'Street' is a suffix street type. There are many.
3. GPO BOX 1285K Melbourne VIC 3001 -- 'G' means it is a corporate box with the same number
available in all Australia Post state branches. 'K' is to be ignored.

Commas are a bugger. They may provide clues as to what should be evaluated together. Or they
might not be there at all. We may find `"12 Elizabeth Street"` or `"12 Elizabeth Street, Malvern"`
or `"12 Elizabeth Street VIC 3144"`.
|-}

type Box = Text

type StreetNumber = Text
type StreetName = Text
type StreetType = Text

type City = Text
type Postcode = Text
type State = Text

data Pobox = Gpo Box | Po Box
  deriving (Show, Eq, Ord)

data StreetAddress = StAddr
  { getStreetNumber :: StreetNumber
  , getStreetName :: StreetName
  , getStreetType :: StreetType
  } deriving (Show, Eq, Ord)

data AddressLocation = APobox Pobox | AStreetAddress StreetAddress
  deriving (Show, Eq, Ord)

data Address = Address
  { getAddressLocation :: AddressLocation
  , getCity :: City
  , getState :: State
  , getPostcode :: Postcode
  } deriving (Show, Eq, Ord)

addressLocation :: Parser AddressLocation
addressLocation =
  (poBox >>= return . APobox)
  <|> (streetAddress >>= return . AStreetAddress)

poBox :: Parser Pobox
poBox = do
  g <- try $ text "g" <|> text ""
  _ <- spaceOrStop
  p <- char 'p'
  _ <- spaceOrStop
  o <- char 'o'
  _ <- spaceOrStop
  box <- text "box"
  _ <- spaceOrStop
  n <- postcode
  _ <- spaceOrStop
  _ <- skipOptional k
  case g of
    "g" -> return $ Gpo $ T.pack n
    _ -> return $ Po $ T.pack n

k :: Parser Text
k = try $ text "k " <|> text "k,"

streetAddress :: Parser StreetAddress
streetAddress = do
  n <- streetNumber -- simple case; we'll improve later with ranges, suffixes, etc.
  _ <- spaceOrComma
  sn <- takeUntil (spaceOrComma >> aStreetType) -- street name
  _ <- spaceOrComma
  t <- aStreetType
  -- c <- takeUntil (try $ text "," <|> try aState <|> postcode) -- to use with 'the'
  return $ StAddr n sn t

main :: IO ()
main = hspec $ do
  describe "Post Office boxes" $ do
    it "PO box" $ do
      let (Success n) = parseByteString poBox mempty "po box 1234"
      n `shouldBe` Po "1234"
    it "GPO box" $ do
      let (Success n) = parseByteString poBox mempty "gpo box 1234"
      n `shouldBe` Gpo "1234"
    it "skips the trailing 'k'" $ do
      let (Success n) = parseByteString poBox mempty "gpo box 1234 k,"
      n `shouldBe` Gpo "1234"
  describe "Street addresses" $ do
    it "Simple case" $ do
      let (Success n) = parseByteString streetAddress mempty "12 fair view road"
      n `shouldBe` (StAddr "12" "fair view" "road")
  describe "Choose best fit" $ do
    it "chooses street addresses" $ do
      let (Success n) = parseByteString addressLocation mempty "12 fair view road"
      n `shouldBe` (AStreetAddress $ StAddr "12" "fair view" "road")
    it "chooses po boxes" $ do
      let (Success n) = parseByteString addressLocation mempty "gpo box 1234 k"
      n `shouldBe` (APobox $ Gpo "1234")
    
