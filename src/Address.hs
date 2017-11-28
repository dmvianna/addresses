
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Address where

import           Control.Applicative
import           Data.ByteString       (ByteString)
import           Data.Char
import qualified Data.Text             as T
import Data.Text.Encoding
import Data.Text (Text)
import           Test.Hspec
import           Text.Parser.LookAhead
import           Text.RawString.QQ
import           Text.Trifecta

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

exampleAddress :: ByteString
exampleAddress = "12 nice view road, narre warren east, vic 3011"

-- parseByteString (commaSep $ many $ notChar ',') mempty exampleAddress

commaList :: TokenParsing m => m [String]
commaList = commaSep $ many $ notChar ','

cardinalPoints :: CharParsing m => [m Text]
cardinalPoints = text <$> ["north","south","east","west","nth","sth","e","w","n","s"]

states :: CharParsing m => [m Text]
states = text <$> ["nsw","tas","nt","wa","qld","sa","vic"
                  ,"new south wales","tasmania","northern territory"
                  ,"western australia","south australia","victoria"]

streetTypes :: CharParsing m => [m Text]
streetTypes = text <$> ["street","drive","avenue","road","lane","highway","parade"]

aPoint :: Parser Text
aPoint = choice cardinalPoints

aState :: Parser Text
aState = choice states

aStreetType :: Parser Text
aStreetType = choice streetTypes

exampleStatePostcode :: ByteString
exampleStatePostcode = "vic 3010"

postcodeEOF :: Parser String
postcodeEOF = do
  p <- count 4 digit
  _ <- eof
  return p

postcodeS :: Parser String
postcodeS = do
  p <- count 4 digit
  _ <- lookAhead $ noneOf "0123456789"
  return p

postcode :: Parser String
postcode = postcodeEOF <|> postcodeS 

exampleSuburb :: ByteString
exampleSuburb = "narre warren east"

-- T.words (decodeLatin1 exampleSuburb)

foundPoint :: Parser Text
foundPoint = do
  p <- aPoint
  _ <- eof
  return p

the :: Parser Text
the = text "the"

streetNumber :: Parser Text
streetNumber = many digit >>= \digits ->
  if length digits > 4
  then fail "Too many digits lol"
  else pure (T.pack digits)

-- tests

main :: IO ()
main = hspec $ do
  describe "street number" $ do
    it "parses 4 digits" $ do
      let (Success n) = parseByteString streetNumber mempty "1234B"
      n `shouldBe` "1234"
    -- it "fails on 5 digits" $ do
    --   case parseByteString streetNumber mempty "12345" of
    --     Success _ -> fail "succeeded on 5 digits. That shouldn't happen."
    --     Failure _ -> pure "success. move along."
