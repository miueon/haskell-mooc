module Collatz where

step :: Integer -> Integer
step x = if even x then down else up
  where
    down = div x 2
    up = 3 * x + 1

collatz :: Integer -> Integer
collatz 1 = 0
collatz x = 1 + collatz (step x)

longest :: Integer -> Integer
longest upperBound = longest' 0 0 upperBound

longest' :: Integer -> Integer -> Integer -> Integer
longest' number _ 0 = number
longest' number maxlength n =
  if length > maxlength
    then longest' n length (n - 1)
    else longest' number maxlength (n -1)
  where
    length = collatz n