

module Locality where

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

newtype Suburb = Suburb Text deriving (Show, Eq, Ord)
newtype State = State Text deriving (Show, Eq, Ord)
newtype Postcode = Postcode Text deriving (Show, Eq, Ord)

data Locality = Locality
  { _suburb   :: Suburb
  , _state    :: State
  , _postcode :: Postcode
  } deriving (Show, Eq, Ord)

locality :: Parser Locality
locality = do
  sb <- takeUntilN 32 (spaceOrComma' >> aState >> spaceOrComma')
  _ <- spaceOrComma'
  st <- aState
  _ <- spaceOrComma'
  p <- postcode
  pure $ Locality (Suburb sb) (State st) (Postcode p)

