import Data.Numbers.Primes

checkNum::Int->Int->Bool
checkNum num commonMultiple = if mod commonMultiple num == 0 then True else False

getMultiple::Int->Int->Int
getMultiple num commonMultiple = div num $ mod commonMultiple num

myWorker::Int->Int->Int
myWorker 20 commonMultiple = commonMultiple
myWorker ptr commonMultiple = if checkNum ptr commonMultiple then myWorker (ptr+1) commonMultiple else myWorker (ptr+1) (commonMultiple*(getMultiple ptr commonMultiple))

problem5::Int
problem5 = myWorker 2 $ product $ takeWhile (<21) primes