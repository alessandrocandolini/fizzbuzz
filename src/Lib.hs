{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Lib where

import Control.Monad.Except
import Data.Either.Combinators (maybeToRight)
import Data.Graph (edges)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import qualified System.Exit as S
import qualified Text.Read as T

data FizzBuzz = Fizz | Buzz | FizzBuzz | Other Integer deriving (Eq, Show)

data Error = NaN | NegativeNumber deriving (Eq, Show)

fizzBuzz :: Integer -> FizzBuzz
fizzBuzz n
  | n `mod` 3 == 0 && n `mod` 5 == 0 = FizzBuzz
  | n `mod` 3 == 0 = Fizz
  | n `mod` 5 == 0 = Buzz
  | otherwise = Other n

render :: FizzBuzz -> String
render (Other n) = show n
render s = show s

generateList :: Integer -> Maybe (N.NonEmpty Integer)
generateList = (=<<) N.nonEmpty . fmap (\n -> [1 .. n]) . mfilter (> 0) . Just

businessLogic :: String -> Either Error (NonEmpty FizzBuzz)
businessLogic = fmap (fmap fizzBuzz) . (=<<) (maybeToRight NegativeNumber . generateList) . maybeToRight NaN . T.readMaybe

source :: IO String
source = putStrLn "insert a positive number" >> getLine

{-
 Approach 1

  - Pros: testability, I can test almost the entire program as a single error
  - Cons: untyped, and we have already run the error effect, so we can't add further modifications only in case of error (eg, exitSuccess or exitFailure)

-}

presentation :: Either Error (NonEmpty FizzBuzz) -> String
presentation = either renderError renderSuccess

errorNaN :: String
errorNaN = "insert a valid number"

errorNegativeNumber :: String
errorNegativeNumber = "insert a number > 0"

renderError :: Error -> String
renderError NaN = errorNaN
renderError NegativeNumber = errorNegativeNumber

renderSuccess :: NonEmpty FizzBuzz -> String
renderSuccess = intercalate "\n" . N.toList . fmap render

program' :: String -> String
program' = presentation . businessLogic

{-
 Approach 2
  - Pros: we run the error effect at the edges
  - Cons: less testable, because now we return IO

 An interemdiate approach could be to "Render" already to string, but not collapse the error effect

-}
program'' :: String -> IO ()
program'' = either outputError' (output' . fmap render) . businessLogic
  where
    outputError' s = outputError s >> S.exitFailure
    output' s = output s >> S.exitSuccess

output :: NonEmpty String -> IO ()
output = mapM_ putStrLn

outputError :: Error -> IO ()
outputError = putStrLn . renderError

program1 :: IO ()
program1 = (=<<) putStrLn $ fmap program' source

program2 :: IO ()
program2 = (=<<) program'' source

program :: IO ()
program = program2
