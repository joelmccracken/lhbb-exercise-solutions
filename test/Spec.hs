{-# LANGUAGE OverloadedStrings #-}

import RIO

import RIO.HashMap as HM
import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "seriousBusiness" $ do
    it "normalizes casing of needles" $ do
      seriousBusiness "the pen is blue" "BLUE,black" `shouldBe`
        Results (HM.fromList [("blue", 1)]) 3

    it "handles the provided examples" $ do
      seriousBusiness "I'm Blue abade" "blue" `shouldBe`
        Results (HM.fromList [("blue", 1)]) 3
      seriousBusiness "sacred" "red" `shouldBe` Results (HM.fromList []) 1
      seriousBusiness "SpOoKy sCaRy sKeLeToNs" "scary" `shouldBe`
        Results (HM.fromList [("scary", 1)]) 2
      seriousBusiness "SpOoKy sCaRy sKeLeToNs" "SCARY" `shouldBe`
        Results (HM.fromList [("scary", 1)]) 2
      seriousBusiness "Jack the pumpkinking." "pumpkin" `shouldBe`
        Results (HM.fromList []) 3
      seriousBusiness "Carve a pumpkin." "pumpkin" `shouldBe`
        Results (HM.fromList [("pumpkin", 1)]) 2

  describe "inHaystack" $ do
    it "should work" $ do
      inHaystack ["a", "b"] "b" `shouldBe` True
      inHaystack ["a", "b"] "c" `shouldBe` False

  describe "prepareHaystack" $ do
    it "normalizes case" $ do
      prepareHaystack "Hello World" `shouldBe` ["hello", "world"]

    it "splits out a bunch of non-word characters, respecting boundaries" $ do
      prepareHaystack "what.?\" up" `shouldBe` ["what", "up"]

