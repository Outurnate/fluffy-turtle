module Moar
  where
  signum x =
    if x < 0
      then -1
      else if x > 0
      then 1
      else 0
  roots a b c =
    let det = sqrt (b * b - 4 * a * c)
        twicea = 2 * a
    in ((-b + det) / twicea,
        (-b - det) / twicea)
  fib 0 = 1
  fib 1 = 1
  fib n = fib (n - 2) + fib (n - 1)
    