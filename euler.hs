import Utils

import List (sort, nub, group)
import Data.List (permutations)
import Data.Digits
import Data.Numbers.Primes (primes, primeFactors)

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

euler44 =
  {- Find the smallest pair of pentagonal numbers whose sum and difference is pentagonal. -}
  let isgood x y = (abs (x - y)) `elemorder` pentagonals &&
                   (x + y) `elemorder` pentagonals
      hasPentDiff x =
        let pentsBelow = takeWhile (< x) pentagonals
        in filter (not . null) $ map (\y -> if (isgood x y) then [x, y] else []) pentsBelow
      (x:y:_) = flatten $ head $ filter (not . null) $ map hasPentDiff pentagonals
  in x - y

euler46 =
  {- What is the smallest odd composite that cannot be written as the sum of a prime and
     twice a square?
  -}
  let oddComposites = tail [x | x <- composites, odd x]
      twiceSquares = [2*x^2 | x <- [1..]]
      isgood n = let primesBelow = takeWhile (< n) primes
                     squaresBelow = takeWhile (< n) twiceSquares
                     allcombs = [[x, y] | x <- primesBelow, y <- squaresBelow]
                 in any (\(x:y:_) -> x + y == n) allcombs
  in head $ filter (not . isgood) oddComposites

euler47 =
  {- Find the first four consecutive numbers with 4 pairwise distinct
     prime factors.
  -}
  let getFactors n = group $ primeFactors n
      isgood a b c d =
        let withDupes = map getFactors [a, b, c, d]
            lens = map length withDupes
        in length withDupes == 4 && all (== 4) lens
      findMatch (a:b:c:d:xs) = if isgood a b c d then a else findMatch $ b:c:d:xs
  in findMatch [1..]

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