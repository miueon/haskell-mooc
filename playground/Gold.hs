module Gold where

phi = (sqrt 5 + 1) / 2

polynomial x = x ^ 2 - x -1

f x = polynomial (polynomial x)

increment x =
  let x = x + 1
   in x

main = do
  print (polynomial phi)
  print (f phi)