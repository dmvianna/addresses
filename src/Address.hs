
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
Addresses are not standardised. Here is a not comprehensive list
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
  -- _ <- try $ lookAhead $ noneOf "k " <|> char 'k' >> oneOf " ," <|> text ""
  case g of
    "g" -> return $ Gpo $ T.pack n
    _ -> return $ Po $ T.pack n

main :: IO ()
main = hspec $ do
  describe "Post Office boxes" $ do
    it "PO box" $ do
      let (Success n) = parseByteString poBox mempty "po box 1234"
      n `shouldBe` Po "1234"
    it "GPO box" $ do
      let (Success n) = parseByteString poBox mempty "gpo box 1234"
      n `shouldBe` Gpo "1234"
    
