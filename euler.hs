import Utils

import List (sort, nub)
import Data.List (permutations)
import Data.Digits
import Data.Numbers.Primes (primes)

is43pandigital (d1:d2:d3:d4:d5:d6:d7:d8:d9:d10:xs) =
      unDigits 10 [d8, d9, d10] `mod` 17 == 0 &&
      unDigits 10 [d7, d8, d9] `mod` 13 == 0 &&
      unDigits 10 [d6, d7, d8] `mod` 11 == 0 &&
      unDigits 10 [d5, d6, d7] `mod` 7 == 0 &&
      unDigits 10 [d4, d5, d6] `mod` 5 == 0 &&
      unDigits 10 [d3, d4, d5] `mod` 3 == 0 &&
      unDigits 10 [d2, d3, d4] `mod` 2 == 0
      
euler43 :: Integer
euler43 = sum $ map (unDigits 10) $ filter is43pandigital $ permutations [0..9]

euler49 =
  let fourDigitPrimes = filter (>999) $ takeWhile (<9999) primes
      getPermsInP n p = filter (\x -> x `elem` p) $ map (unDigits 10) $ permutations $ digits 10 n
      isgood (x:y:z:_) p = (y - x) == (z - y) && x /= y && y /= z && all (\x -> x `elem` p) [x,y,z]
      otherSeq = last $ nub $ concat $ filter (not . null) $ 
         map (\y -> filter (\x -> isgood x fourDigitPrimes) 
                    $ map sort $ combinations 3 
                    $ getPermsInP y fourDigitPrimes) fourDigitPrimes
         in concatNums otherSeq

euler50 = 
  let p = takeWhile (< 1000000) primes
      aux n = maximum $ filter (\x -> x `elem` p) $ takeWhile (< 1000000) $ scanl1 (+) $ drop n primes
      in maximum $ map aux [1..10]

euler55 = length $ filter id $ map lychrel [1..9999]