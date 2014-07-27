import Data.Numbers.Primes

getGoal::(Integral a)=> a -> a
getGoal number = number

myWorker::(Integral a)=> Int -> a -> a
myWorker ptr number = if mod number primeNum == 0 then if isPrime divNum then getGoal divNum else myWorker (ptr+1) divNum else myWorker (ptr+1) number
	where primeNum = last $ take ptr primes
	      divNum = div number primeNum