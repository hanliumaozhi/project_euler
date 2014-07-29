import Data.Numbers.Primes

problem10::Int
problem10 = sum $ takeWhile (<2000000) primes