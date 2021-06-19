module Lib where

import Control.Monad.Except
import Data.Either.Combinators (maybeToRight)
import qualified System.Exit as S
import qualified Text.Read as T

program :: IO ()
program = runExceptT program' >>= either outputError' output'
  where
    outputError' s = outputError s >> S.exitFailure
    output' s = output s >> S.exitSuccess

program' :: ExceptT Error IO [String]
program' = fmap presentation source

data FizzBuzz = Fizz | Buzz | FizzBuzz | Other Integer deriving (Eq, Show)

newtype Seed = Seed Integer deriving (Eq, Show)

seed :: Integer -> Maybe Seed
seed = fmap Seed . mfilter (> 0) . Just

data Error = NaN | NegativeNumber deriving (Eq, Show, Ord)

fizzBuzz :: Integer -> FizzBuzz
fizzBuzz n
  | n `mod` 3 == 0 && n `mod` 5 == 0 = FizzBuzz
  | n `mod` 3 == 0 = Fizz
  | n `mod` 5 == 0 = Buzz
  | otherwise = Other n

render :: FizzBuzz -> String
render (Other n) = show n
render s = show s

renderError :: Error -> String
renderError NaN = "insert a valid number"
renderError NegativeNumber = "insert a number > 0"

business :: Seed -> [FizzBuzz]
business (Seed n) = fmap fizzBuzz [1 .. n]

presentation :: Seed -> [String]
presentation = fmap render . business

rawSource :: IO String
rawSource = putStrLn "insert a positive number" >> getLine

source :: ExceptT Error IO Seed
source = ExceptT $ fmap parseAndValidate rawSource
  where
    parseAndValidate = (=<<) validate . parse

parse :: String -> Either Error Integer
parse = maybeToRight NaN . T.readMaybe

validate :: Integer -> Either Error Seed
validate = maybeToRight NegativeNumber . seed

output :: [String] -> IO ()
output = mapM_ putStrLn

outputError :: Error -> IO ()
outputError = putStrLn . renderError
