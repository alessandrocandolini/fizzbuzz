module Lib where

import Control.Monad.Except (ExceptT (..), mfilter, runExceptT)
import Data.Either.Combinators (maybeToRight)
import qualified System.Exit as S
import qualified Text.Read as T

program :: IO ()
program = runExceptT program' >>= either outputError' output'
  where
    outputError' s = outputError s >> S.exitFailure
    output' s = output s >> S.exitSuccess

program' :: ExceptT Error IO [String]
program' = do
  s <- ExceptT $ validateInput <$> input
  return $ calculate s

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

renderError :: Error -> String
renderError NaN = "insert a valid number"
renderError NegativeNumber = "insert a number > 0"

calculate :: Integer -> [String]
calculate n = fmap (render . fizzBuzz) [1 .. n]

input :: IO String
input = putStrLn "insert a positive number" >> getLine

validateInput :: String -> Either Error Integer
validateInput s = validateNumber s >>= validatePositive
  where
    validateNumber = maybeToRight NaN . T.readMaybe
    validatePositive = maybeToRight NegativeNumber . mfilter (> 0) . Just

output :: [String] -> IO ()
output = mapM_ putStrLn

outputError :: Error -> IO ()
outputError = putStrLn . renderError
