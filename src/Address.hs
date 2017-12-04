
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Address where

import           Control.Applicative
import           Data.ByteString       (ByteString)
import           Data.Char
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Text.Encoding
import           Test.Hspec
import           Text.Parser.LookAhead
import           Text.RawString.QQ
import           Text.Trifecta

import           Components            hiding (main)

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
  , getStreetName   :: StreetName
  , getStreetType   :: StreetType
  } deriving (Show, Eq, Ord)

data AddressLocation = APobox Pobox | AStreetAddress StreetAddress
  deriving (Show, Eq, Ord)

data Address = Address
  { getAddressLocation :: AddressLocation
  , getCity            :: City
  , getState           :: State
  , getPostcode        :: Postcode
  } deriving (Show, Eq, Ord)

addressLocation :: Parser AddressLocation
addressLocation =
  try (poBox >>= return . APobox)
  <|> (streetAddress >>= return . AStreetAddress)

step :: Parser AddressLocation
step = do
  _ <- anyChar
  try addressLocation <|> step

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
    "g" -> return $ Gpo n
    _   -> return $ Po n

k :: Parser Text
k = try $ text "k " <|> text "k,"

streetAddress :: Parser StreetAddress
streetAddress = do
  n <- streetNumber -- simple case; we'll improve later with ranges, suffixes, etc.
  _ <- spaceOrComma
  th <- optional the
  case th of
    Just t -> do
      _ <- spaces
      sn <- takeUntil (try $ text "," <|> try aState <|> postcode)
      return $ StAddr n sn t
    Nothing -> do
      sn <- takeUntil (spaceOrComma >> aStreetType) -- street name
      _ <- spaceOrComma
      t <- aStreetType
      return $ StAddr n sn t

