getMaxMutiple::(Integral a)=> a -> a -> a
getMaxMutiple item limit = limit `div` item

getMutipleSum::(Integral a)=> a -> a -> a
getMutipleSum item limit = (item * (1 + times)*times) `div` 2
							where 
								times = getMaxMutiple item limit
								

problem1::(Integral a)=> a
problem1 = getMutipleSum 3 1000 + getMutipleSum 5 1000 - getMutipleSum 15 1000