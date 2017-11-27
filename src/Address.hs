{-# LANGUAGE OverloadedStrings #-}

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

aPoint :: Parser Text
aPoint = choice cardinalPoints

aState :: Parser Text
aState = choice states

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
