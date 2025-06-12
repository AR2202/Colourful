{-# LANGUAGE OverloadedStrings #-}
import Eval
import Parser
import Transpiler
import Test.Hspec
import Test.QuickCheck
main :: IO ()
main = hspec $
    do
      -- testing Parser
      parseYellowTest
      parseYellowSentenceTest
      parseColourStringsTest
      parseColourStringsTest2

      -- testing transpiler
      transpileRedGreenTest
      transpileDefAndUseTest
      -- eval
      evalYellowTest
      evalSKITest
      evalKSITest
      evalKIIKTest
      evalparensTest

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
-- | expectation for test that Yellow is parsed in a sentence
parseColourStringsExp :: Expectation
parseColourStringsExp =
  parseColourstrings "Red Green and Black Blue NewColour White and Yellow"
    `shouldBe` Right [" and Yellow","Black Blue NewColour White","Red Green and "]

-- |  test that colour definition strings are separated
-- | appropriately from other strings
parseColourStringsTest :: SpecWith ()
parseColourStringsTest =
  describe "parseInsert2SKI" $

      it
        "should separate the definition from the rest"
        parseColourStringsExp
-- | expectation for test that Yellow is parsed in a sentence
parseColourStringsExp2 :: Expectation
parseColourStringsExp2 =
  parseColourstrings "call False and True defining False:\nBlack starts the definition and assigns Orange to False White ends the definition. \nThe next definition we need is Black starts Red is defined as True White ends"
    `shouldBe` Right ( reverse ["call False and True defining False:\n","Black starts the definition and assigns Orange to False White" ," ends the definition. \nThe next definition we need is ","Black starts Red is defined as True White"," ends"])

-- |  test that colour definition strings are separated
-- | appropriately from other strings
parseColourStringsTest2 :: SpecWith ()
parseColourStringsTest2 =
  describe "parseInsert2SKI" $

      it
        "should separate the definition from the rest"
        parseColourStringsExp2
-- transpiler
--------------
-- | expectation for test that Red Green and Black is correctly tranpiled
transpileRedGreenExp :: Expectation
transpileRedGreenExp =
  parseInsert2SKI colourDict "Red Green and Black"
    `shouldBe` Right (App K (App S I))

-- |  test that Red Green and Black is correctly tranpiled
transpileRedGreenTest :: SpecWith ()
transpileRedGreenTest =
  describe "parseInsert2SKI" $
    context "when parsing \"Red Green and Black\"" $
      it
        "should return Right K(SI)"
        transpileRedGreenExp

transpileDefAndUseExp :: Expectation
transpileDefAndUseExp =
  parseInsert2SKI colourDict "Ocean Black Green Ocean White"
    `shouldBe` Right (App S I)

-- |  test that Red Green and Black is correctly tranpiled
transpileDefAndUseTest :: SpecWith ()
transpileDefAndUseTest =
  describe "parseInsert2SKI" $
    context "when parsing a definition and use of a new colour" $
      it
        "should return the transpiled definition of the new colour"
        transpileDefAndUseExp
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

evalparensExp :: Expectation
evalparensExp =
  eval  (App(App K I)(App I K))
    `shouldBe` I

-- |  test that KIIK is evaluated to K
evalparensTest :: SpecWith ()
evalparensTest =
  describe "eval" $
    context "when evaluating \"KI(IK)\"" $
      it
        "should return I"
        evalparensExp