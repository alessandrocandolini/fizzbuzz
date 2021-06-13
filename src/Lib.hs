module Lib where

program :: IO ()
program = input >>= output . calculate

data FizzBuzz = Fizz | Buzz | FizzBuzz | Other Integer deriving (Eq, Show)

fizzBuzz :: Integer -> FizzBuzz
fizzBuzz n
  | n `mod` 3 == 0 && n `mod` 5 == 0 = FizzBuzz
  | n `mod` 3 == 0 = Fizz
  | n `mod` 5 == 0 = Buzz
  | otherwise = Other n

printFizzBuzz :: FizzBuzz -> String
printFizzBuzz (Other n) = show n
printFizzBuzz s = show s

calculate :: Integer -> [String]
calculate n = fmap (printFizzBuzz . fizzBuzz) [1 .. n]

-- todo typed validation (exceptT)
input :: IO Integer
input = do
  putStrLn "insert number"
  read <$> getLine

output :: [String] -> IO ()
output = mapM_ putStrLn
