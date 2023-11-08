{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
  describe "seriousBusiness" $ do
    it "normalizes casing of needles" $ do
      seriousBusiness "the pen is blue" "BLUE,black" `shouldBe` True

    it "handles the provided examples" $ do
      seriousBusiness "I'm Blue abade" "blue" `shouldBe` True
      seriousBusiness "sacred" "red" `shouldBe` False
      seriousBusiness "SpOoKy sCaRy sKeLeToNs" "scary" `shouldBe` True
      seriousBusiness "SpOoKy sCaRy sKeLeToNs" "SCARY" `shouldBe` True
      seriousBusiness "Jack the pumpkinking." "pumpkin" `shouldBe` False
      seriousBusiness "Carve a pumpkin." "pumpkin" `shouldBe` True

  describe "inHaystack" $ do
    it "should work" $ do
      inHaystack ["a", "b"] "b" `shouldBe` True
      inHaystack ["a", "b"] "c" `shouldBe` False

  describe "prepareHaystack" $ do
    it "normalizes case" $ do
      prepareHaystack "Hello World" `shouldBe` ["hello", "world"]

    it "splits out a bunch of non-word characters, respecting boundaries" $ do
      prepareHaystack "what.?\" up" `shouldBe` ["what", "up"]

