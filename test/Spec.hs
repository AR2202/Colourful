{-# LANGUAGE OverloadedStrings #-}

import Backtranspiler
import Eval
import Parser
import Test.Hspec
import Test.QuickCheck
import Transpiler

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
    transpile2DefsTest
    transpile2DefsSimpleTest
    -- eval
    evalYellowTest
    evalSKITest
    evalKSITest
    evalKIIKTest
    evalparensTest
    -- backtranspiler
    backtranspileSimpleTest
    backtranspileWAppTest
    backtranspileWDefTest
    backtranspileWDefsTest

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
    `shouldBe` Right [" and Yellow", "Black Blue NewColour White", "Red Green and "]

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
    `shouldBe` Right (reverse ["call False and True defining False:\n", "Black starts the definition and assigns Orange to False White", " ends the definition. \nThe next definition we need is ", "Black starts Red is defined as True White", " ends"])

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
    `shouldBe` Right (App (App S I) K)

-- |  test that Red Green and Black is correctly tranpiled
transpileRedGreenTest :: SpecWith ()
transpileRedGreenTest =
  describe "parseInsert2SKI" $
    context "when parsing \"Red Green and Black\"" $
      it
        "should return Right SIK"
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

transpile2DefsExp :: Expectation
transpile2DefsExp =
  parseInsert2SKI colourDict "call False and True defining False:\nBlack starts the definition and assigns Orange to False White ends the definition. \nThe next definition we need is Black starts Red is defined as True White ends"
    `shouldBe` Right (App (App (App K I) K) (App K I))

-- |  test that 2 colour definitions and uses are correctly tranpiled
transpile2DefsTest :: SpecWith ()
transpile2DefsTest =
  describe "parseInsert2SKI" $
    context "when parsing a definition and use of 2 new colours" $
      it
        "should return the transpiled definition of the new colours"
        transpile2DefsExp

transpile2DefsSimpleExp :: Expectation
transpile2DefsSimpleExp =
  parseInsert2SKI colourDict "False True False  Black Red True White Black Orange False White"
    `shouldBe` Right (App (App (App K I) K) (App K I))

-- |  test that 2 colour definitions and uses are correctly tranpiled
transpile2DefsSimpleTest :: SpecWith ()
transpile2DefsSimpleTest =
  describe "parseInsert2SKI" $
    context "when parsing a definition and use of 2 new colours" $
      it
        "should return the transpiled definition of the new colours"
        transpile2DefsSimpleExp

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
  eval (App (App S K) I)
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
  eval (App (App K S) I)
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
  eval (App (App (App K I) I) K)
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
  eval (App (App K I) (App I K))
    `shouldBe` I

-- |  test that KIIK is evaluated to K
evalparensTest :: SpecWith ()
evalparensTest =
  describe "eval" $
    context "when evaluating \"KI(IK)\"" $
      it
        "should return I"
        evalparensExp

-- backtranspiler
-------------------
backtranspileSimpleExp :: Expectation
backtranspileSimpleExp =
  backtranspile colourDict (App (App (App K I) I) K)
    `shouldBe` "RedYellowOrange"

-- |  test that simple SKI expression is
-- | backtranspiled to Colours
backtranspileSimpleTest :: SpecWith ()
backtranspileSimpleTest =
  describe "backtranspile" $
    context "when transpiling a simple SKI expression" $
      it
        "should return the correct colours"
        backtranspileSimpleExp

backtranspileWAppExp :: Expectation
backtranspileWAppExp =
  backtranspile colourDict (App (App K I) (App K I))
    `shouldBe` "OrangeOrange"

-- |  test that simple SKI expression
-- | is backtranspiled to Colours
backtranspileWAppTest :: SpecWith ()
backtranspileWAppTest =
  describe "backtranspile" $
    context "when transpiling a simple SKI expression" $
      it
        "should return the correct colours"
        backtranspileWAppExp

backtranspileWDefExp :: Expectation
backtranspileWDefExp =
  backtranspile colourDict (App (App K I) (App (App K I) S))
    `shouldBe` "BrownOrangeBlack BlueOrange Brown White"

-- |  test that simple SKI expression is backtranspiled to Colours
backtranspileWDefTest :: SpecWith ()
backtranspileWDefTest =
  describe "backtranspile" $
    context "when transpiling an undefined expression" $
      it
        "should create a new colour"
        backtranspileWDefExp

backtranspileWDefsExp :: Expectation
backtranspileWDefsExp =
  backtranspile colourDict (App (App K I) (App (App K I) (App I K )))
    `shouldBe` "BrownOrangeBlack Brown1Orange Brown WhiteBlack RedYellow Brown1 White"

-- |  test that simple SKI expression is backtranspiled to Colours
backtranspileWDefsTest :: SpecWith ()
backtranspileWDefsTest =
  describe "backtranspile" $
    context "when transpiling a nested undefined expression" $
      it
        "should create new colours"
        backtranspileWDefsExp