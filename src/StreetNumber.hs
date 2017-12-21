
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

newtype Suffix = Suffix Text
newtype Prefix = Prefix Text
newtype Number = Number Text

data Single = Single Prefix Suffix Number

data StreetNumber = One Single
                  | Range Single Single

streetNumber :: Parser Text
streetNumber = undefined

singleNumber :: Parser Text
singleNumber = some digit >>= \digits ->
  if length digits > 5
  then fail $ "Too many digits: " <> digits
  else pure (T.pack digits)

singleFix :: Parser Text
singleFix = some letter >>= \letters ->
  if length letters > 2
  then fail $ "Too many letters: " <> letters
  else pure (T.pack letters)

