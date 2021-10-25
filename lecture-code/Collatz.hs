module Collatz where

step x = if even x then down else up
  where
    down = x `div` 2
    up = 3 * x + 1

collatz 1 = 0
collatz x = 1 + collatz (step x) -- pattern matching

longest upperBound = longest' 0 0 upperBound

longest' number _ 0 = number
longest' number maxlength n =
  if length > maxlength
    then longest' n length (n - 1)
    else longest' number maxlength (n -1)
  where
    length = collatz n