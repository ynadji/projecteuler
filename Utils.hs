module Utils where
import Data.Digits
import Data.Numbers.Primes (primes)
import qualified Data.Set as Set
import Data.Bits (shift, bit)

-- Helper functions
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)

concatNums list = read (concat $ map show list) :: Integer

flatten :: [[a]] -> [a]
flatten list = foldl (++) [] list

-- Returns True iff the element is in the _ordered_ list.
elemorder n list = elem n $ takeWhile (<= n) list

memoized_fib :: Int -> Integer
memoized_fib =
   let fib 0 = 0
       fib 1 = 1
       fib n = memoized_fib (n-2) + memoized_fib (n-1)
   in  (map fib [0 ..] !!)

palindrome :: Int -> Bool
palindrome n = digitsRev 10 n == digits 10 n

pandigital :: Int -> [Int] -> Bool
pandigital n list = (Set.fromList list) == (Set.fromList $ digitsRev 10 n)

swapAndSum n = n + unDigits 10 (digitsRev 10 n)

lychrels n = iterate swapAndSum n

lychrel :: Int -> Bool
lychrel n = not $ any palindrome $ take 50 $ drop 1 $ lychrels n

-- Number generators
pentagonals = map (\x -> truncate ((x * (3 * x - 1)) / 2)) [1..]
composites = [x | x <- [1..], not $ x `elemorder` primes]

-- Number theory
isRelPrime x y = gcd x y == 1
totient n = length $ filter (isRelPrime n) [1..n-1]

-- Modular arithmetic
modexp :: Integer -> Integer -> Integer -> Integer
modexp b e m =
  let aux b e m res
        | e <= 0 = res
        | odd e = aux (b * b `mod` m) (shift e (-1)) m (res * b `mod` m)
        | otherwise = aux (b * b `mod` m) (shift e (-1)) m res
  in aux b e m 1