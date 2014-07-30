import ONeillPrimes
import Data.List

checkMod::(Integral a)=>a->a->Bool
checkMod a b = (mod a b) == 0

getSumOfNum::(Integral a)=>a->a
getSumOfNum a = sum [1..a]

getPowerOfNum::(Integral a)=>a->a->a->a
getPowerOfNum a b c = if checkMod a b then getPowerOfNum (div a b) b ( c + 1 ) else c

getPrimeNum::(Integral a)=>Int->a
getPrimeNum ptr = last $ take ptr primes

getPowerList::(Integral a)=>a->Int->[a]->[a]
getPowerList currentNum ptrPrimes primePowerList = if newCurrentNum == 1 then (primePowerList ++ [thePower]) else getPowerList newCurrentNum (ptrPrimes+1) (primePowerList ++ [thePower])
    where thePrime = getPrimeNum ptrPrimes
          thePower = getPowerOfNum currentNum thePrime 0
          newCurrentNum = div currentNum (thePrime^thePower)
		  
myWorker::(Integral a)=>a->a
myWorker a = product $ map (+1) $ getPowerList a 1 []

problemTmp::(Integral a)=>a->a->a
problemTmp numPtr powerNum
    | powerNum > 50 = numPtr
	| otherwise = problemTmp (numPtr+1) (myWorker $ getSumOfNum (numPtr+1))
	


primeFactors n = factor n primes
  where
    factor n (p:ps) 
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      = factor n ps

problem12 = head $ filter ((> 500) . nDivisors) triangleNumbers
  where nDivisors n = product $ map ((+1) . length) (group (primeFactors n))    
        triangleNumbers = scanl1 (+) [1..]



