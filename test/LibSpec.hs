module LibSpec where

import Data.Char (isDigit)
import Data.Either (isRight)
import Data.List (intercalate)
import Data.List.NonEmpty (sort)
import qualified Data.List.NonEmpty as N
import Data.Maybe (fromMaybe, isJust, isNothing)
import Lib
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (NonEmptyList (NonEmpty))
import Test.QuickCheck.Poly (B (B))
import Test.QuickCheck.Property

spec :: Spec
spec = describe "FizzBuzz" $ do
  describe "fizzBuzz" $ do
    prop "fizzbuzz if it's multiple of 3 and 5" $
      \l -> fizzBuzz (3 * 5 * l) == FizzBuzz

    prop "if multiple of 3, it's either fizz or fizzbuzz" $
      \l -> fizzBuzz (3 * l) == FizzBuzz || fizzBuzz (3 * l) == Fizz

    prop "fizz  only if it's multiple of 3 and not of 5" $
      \l -> fizzBuzz l == Fizz ==> l `mod` 3 == 0 && l `mod` 5 /= 0

    prop "if multiple of 5, it's either buzz or fizzBuzz" $
      \l -> fizzBuzz (5 * l) == FizzBuzz || fizzBuzz (5 * l) == Buzz

    prop "buzz only if it's multiple of 5 and not of 3" $
      \l -> fizzBuzz l == Buzz ==> l `mod` 3 /= 0 && l `mod` 5 == 0

    prop "it's other if and only if it's not multiple neither of 3 or 5" $
      \l -> fizzBuzz l == Other l ==> l `mod` 3 /= 0 && l `mod` 5 /= 0

  describe "generateList" $ do
    prop "generates a list whenever the input is greater than 0" $
      \l -> l > 0 ==> isJust $ generateList l

    prop "return empty only if the input is non strict positive" $
      \l -> isNothing (generateList l) ==> l <= 0

    prop "when successful, generates a list of size the same as the input value" $
      \l ->
        l > 0
          ==> let list = maybe [] N.toList $ generateList l
               in length list == fromIntegral l

    prop "when successful, generates sorted lists" $
      \l ->
        let list = generateList l
         in fmap sort list == list

  describe "businessLogic" $ do
    prop "is always successful if the input is a valid strict positive number" $
      \l -> l > 0 ==> isRight $ businessLogic $ show (l :: Integer)

    prop "fails if we can't parse the string" $
      \s -> not (any isDigit s) ==> businessLogic s == Left NaN

    prop "fails if the string is negative" $
      \l -> l <= 0 ==> businessLogic (show (l :: Integer)) == Left NegativeNumber

    it "success example" $
      let expected :: N.NonEmpty FizzBuzz
          expected = Other 1 N.:| [Other 2, Fizz, Other 4, Buzz, Fizz, Other 7, Other 8, Fizz, Buzz, Other 11, Fizz, Other 13, Other 14, FizzBuzz]
       in businessLogic "15" `shouldBe` Right expected

  describe "program'" $ do
    it "success example" $
      let expected :: String
          expected = intercalate "\n" ["1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz", "11", "Fizz", "13", "14", "FizzBuzz"]
       in program' "15" `shouldBe` expected

    it "NaN program" $
      program' "abc" `shouldBe` "insert a valid number"

    it "Negative number program" $
      program' "0" `shouldBe` "insert a number > 0"
