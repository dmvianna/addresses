{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Components where

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

spaceOrStop :: Parser String
spaceOrStop = many $ oneOf " ."

skipUntil :: Parser Text -> Parser Text
skipUntil p = try p <|> T.singleton <$> anyChar >> skipUntil p

takeUntil :: Parser Text -> Parser Text
takeUntil p' =
  go p' T.empty
  where
    go p xs =
      try (lookAhead p) >> return xs <|>
      (anyChar >>= \c -> go p (T.snoc xs c))

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
postcode = try postcodeEOF <|> postcodeS

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
    it "fails on 5 digits" $ do
      let (Failure (ErrInfo errDoc m)) =
            parseByteString streetNumber mempty "12345"
      show m `shouldBe` "[Columns 5 5]"
