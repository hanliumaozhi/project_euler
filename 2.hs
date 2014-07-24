fibstep'::Integral a => (a, a) -> (a, a)
fibstep' (u, v) = (v, u+v)

fibs'::Integral a => a -> [a]
fibs' n = takeWhile (<= n) $ map fst $ iterate fibstep' (1, 2)

problem2::Integral a => a -> a
problem2 n = sum $ filter even $ fibs' n


