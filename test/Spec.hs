{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}

module Main where

import Data.ByteString
import           Text.Trifecta
import Test.Hspec
import           Text.RawString.QQ

import Components hiding (main)
import Address hiding (main)

instance Eq a => Eq (Result a) where
  Success x == Success y = x == y
  _ == _ = False
  
addEx :: ByteString
addEx = [r|abstract (11 document no. au-a-10803/92 (19) australian patent office (54) title film cartridge bar code scanner and controller for a digital imaging system international patent classification(s) (51) 5 g03b007/24 g06k009/18 (21) application no. 10803/92 (22) application date 06.02.92 priority data (31) number (32) date (33) country 656605 19.02.91 us united states of america (43) publication date 27.08.92 (71) applicant(s) minnesota mining and manufacturing company (72) inventor(s) richard randall lemberger; terrence harold joyce (74) attorney or agent spruson ferguson, gpo box 3898, sydney nsw 2001 (57) claim 1. a laser imaging system, comprising: a cartridge of photographic film; a machine readable information bearing medium associated with the cartridge and including information characterizing the cartridge and/or film; a laser imager, including: a cartridge receiving mechanism; a laser scanning system including a laser for imaging the film; and a reading device for reading the information from the information bearing medium; and an image management system responsive to image input data and coupled to the laser imager, for controlling the laser imager as a function of the input data and the information read from the information bearing medium. i i|]

main :: IO ()
main = hspec $ do
  describe "takeUntil" $ do
    it "accumulates all characters until the argument parser succeeds" $ do
      let (Success x) = parseByteString (takeUntil $ text "road") mempty "12 fair view road"
      x `shouldBe` "12 fair view "
  describe "street number" $ do
    it "parses 4 digits" $ do
      let (Success n) = parseByteString streetNumber mempty "1234B"
      n `shouldBe` "1234"
    it "fails on 5 digits" $ do
      let (Failure (ErrInfo errDoc m)) =
            parseByteString streetNumber mempty "12345"
      show m `shouldBe` "[Columns 5 5]"

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
    it "'the' street type" $ do
      let (Success n) = parseByteString streetAddress mempty "12 the promenade, nsw"
      n `shouldBe` (StAddr "12" "promenade" "the")
  describe "Choose best fit" $ do
    it "chooses street addresses" $ do
      let (Success n) = parseByteString addressLocation mempty "12 fair view road"
      n `shouldBe` (AStreetAddress $ StAddr "12" "fair view" "road")
    it "chooses po boxes" $ do
      let (Success n) = parseByteString addressLocation mempty "gpo box 1234 k"
      n `shouldBe` (APobox $ Gpo "1234")

  describe "large example" $ do
    it "finds PO Box within text" $ do
      let expected = APobox $ Gpo "3898" --, sydney nsw 2001
      let actual = parseByteString addressLocation mempty addEx
      actual `shouldBe` Success expected
