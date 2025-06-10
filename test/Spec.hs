{-# LANGUAGE OverloadedStrings #-}
import Eval
import Parser
import Test.Hspec
import Test.QuickCheck
main :: IO ()
main = hspec $
    do
      -- testing Parser
      parseYellowTest
      parseYellowSentenceTest
      -- eval
      evalYellowTest
      evalSKITest
      evalKSITest
      evalKIIKTest

-- | expectation for test that Yellow is parsed
parseYellowExp :: Expectation
parseYellowExp =
  parseColour "Yellow"
    `shouldBe` Right (ColourUse "Yellow")

-- |  test that Yellow is parsed as Yellow
parseYellowTest :: SpecWith ()
parseYellowTest =
  describe "parseColour" $
    context "when parsing \"Yellow\"" $
      it
        "should return Right (ColourUse Yellow)"
        parseYellowExp

-- | expectation for test that Yellow is parsed in a sentence
parseYellowSentenceExp :: Expectation
parseYellowSentenceExp =
  filter isColourUse <$> parseColours "This colour is Yellow"
    `shouldBe` Right [ColourUse "Yellow"]

-- |  test that Yellow is parsed as Yellow
parseYellowSentenceTest :: SpecWith ()
parseYellowSentenceTest =
  describe "parseColours" $
    context "when parsing \"Yellow\" in a sentence" $
      it
        "should return Right (ColourUse Yellow)"
        parseYellowSentenceExp
-- eval
------------
evalYellowExp :: Expectation
evalYellowExp =
  eval I
    `shouldBe` I

-- |  test that I is evaluated to I
evalYellowTest :: SpecWith ()
evalYellowTest =
  describe "eval" $
    context "when evaluating \"I\"" $
      it
        "should return I"
        evalYellowExp

evalSKIExp :: Expectation
evalSKIExp =
  eval  (App(App S K)I)
    `shouldBe` I

-- |  test that I is evaluated to I
evalSKITest :: SpecWith ()
evalSKITest =
  describe "eval" $
    context "when evaluating \"SKI\"" $
      it
        "should return I"
        evalSKIExp

evalKSIExp :: Expectation
evalKSIExp =
  eval  (App(App K S)I)
    `shouldBe` S

-- |  test that KSI is evaluated to S
evalKSITest :: SpecWith ()
evalKSITest =
  describe "eval" $
    context "when evaluating \"KSI\"" $
      it
        "should return S"
        evalKSIExp

evalKIIKExp :: Expectation
evalKIIKExp =
  eval  (App(App(App K I)I)K)
    `shouldBe` K

-- |  test that KIIK is evaluated to K
evalKIIKTest :: SpecWith ()
evalKIIKTest =
  describe "eval" $
    context "when evaluating \"KIIK\"" $
      it
        "should return K"
        evalKIIKExp