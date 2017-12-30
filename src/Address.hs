
module Address where

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
import           Locality
import           StreetNumber

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

newtype StreetName = StreetName Text deriving (Show, Eq, Ord)
newtype StreetType = StreetType Text deriving (Show, Eq, Ord)

type Box = Text
data Pobox = Gpo Box | Po Box
  deriving (Show, Eq, Ord)

data Bag = Locked Box | Private Box
  deriving (Show, Eq, Ord)

data StreetAddress = StAddr
  { _streetNumber :: StreetNumber
  , _streetName   :: StreetName
  , _streetType   :: StreetType
  } deriving (Show, Eq, Ord)

data AddressLocation = APobox Pobox | AStreetAddress StreetAddress
  deriving (Show, Eq, Ord)

data Address = Address
  { _addressLocation :: AddressLocation
  , _suburb          :: Suburb
  , _postcode        :: Postcode
  , _state           :: State
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

bag :: Parser Bag
bag = do
  bt <- try $ text "locked" <|> text "private"
  _ <- spaceOrStop
  _ <- text "bag"
  _ <- spaceOrStop
  _ <- skipOptional $ (try (text "no") <|> text "n")
  _ <- spaceOrStop
  bn <- T.pack <$> some digit
  case bt of
    "locked"  -> return $ Locked bn
    "private" -> return $ Private bn
    _         -> unexpected $ "Locked or Private bag returned: " <> T.unpack bt

streetAddress :: Parser StreetAddress
streetAddress = do
  n <- streetNumber -- simple case; we'll improve later with ranges, suffixes, etc.
  _ <- spaceOrComma
  th <- optional the
  case th of
    Just t -> do
      _ <- spaces
      sn <- takeUntilN 32 (try $ text ","
                            <|> try (spaceOrComma' >> aState >> spaceOrComma')
                            <|> (spaceOrComma' >> postcode >> spaceOrComma')
                          )
      street n sn t
    Nothing -> do
      sn <- takeUntilN 32 $ boundedStreetType -- street name
      _ <- spaceOrComma
      t <- aStreetType
      street n sn t
  where
    street n' sn' t' =
      if T.length sn' > 0
      then pure $ StAddr n' (StreetName sn') (StreetType t')
      else unexpected "street with no name"
    boundedStreetType =
      spaceOrComma >> aStreetType >> spaceOrComma'


data AuAddress = AuAddress
  { _addrLocation :: AddressLocation
  , _addrLocality :: Locality
  }
  deriving (Show, Eq, Ord)

auAddress :: Parser AuAddress
auAddress = do
  alocn <- addressLocation
  _ <- spaceOrComma'
  alocl <- locality
  pure $ AuAddress { _addrLocation = alocn
                   , _addrLocality = alocl }
