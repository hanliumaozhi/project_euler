fa::Int->Int->Bool
fa x y = (x+y) < 667

myWorker::(Int, Int)
myWorker = [(a,b) | a <- [1..333], b<-[1..500], fa a b, a^2+b^2 == (1000-a-b)^2] !! 0

problem9::Int
problem9 = (fst numTuple) * (snd numTuple) * (1000 - (fst numTuple) - (snd numTuple))
	where numTuple = myWorker