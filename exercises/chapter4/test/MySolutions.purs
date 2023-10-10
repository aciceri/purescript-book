module Test.MySolutions where

import Prelude
import Data.Semigroup ((<>))
import Data.Array (head, length, tail, filter, (..), last)
import Data.Int (rem)
import Data.Maybe (fromMaybe)
import Test.Examples (factorsV3)
import Control.Alternative (guard)


abs n = if n < 0 then -n else n

isEven :: Int -> Boolean
isEven n = if n == 0 then true
           else if n == 1 then false
           else isEven <<< abs $ n - 2

countEven :: Array Int -> Int
countEven a = if length a == 0
              then 0
              else (if isEven (fromMaybe 0 (head a)) then 1 else 0) + countEven (fromMaybe [] (tail a))

square :: Number -> Number
square n = n * n

squared :: Array Number -> Array Number
squared = map square

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (flip (>=) 0.0)

infixr 8 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite a = (flip (>=) 0.0) <$?> a

isPrime :: Int -> Boolean
isPrime n = n > 1 && let
  factors = factorsV3 n
  in length factors == 1 

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct a b = do
  x <- a
  y <- b
  pure [x, y]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard (a * a + b * b == c * c)
  pure [a, b, c]

divide :: Int -> Int -> Boolean
divide n d = rem n d == 0

primeDivisors :: Int -> Array Int
primeDivisors n = do
  ds <- divide n <$?> isPrime <$?> (1 .. n)
  pure ds

primeFactors :: Int -> Array Int
primeFactors n = if n == 1
                 then []
                 else let
                   ds = primeDivisors n
                   g = fromMaybe 0 $ head ds
                   in [g] <> primeFactors (n `div` g)
                   
