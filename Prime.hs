import Data.Time

main :: IO ()
main = do
  x <- getCurrentTime
  let cnt = countPrime 1000000
  y <- getCurrentTime
  print cnt
  print $ diffUTCTime y x

countPrime :: Int -> Int
countPrime n = length (filter isPrime [1 .. n])

isPrime :: Int -> Bool
isPrime n
  | n == 2 = True
  | n < 2 || even n = False
  | otherwise =
    True `notElem` map (\m -> mod n m == 0) [3 .. ceiling (sqrt (fromIntegral n))]
