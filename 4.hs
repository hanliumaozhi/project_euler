checkNum::Int->Bool
checkNum a = (reverse $ show a) == (show a)

problem4::Int
problem4 = maximum $ filter (\x -> checkNum x) [x*y|x<-[1..999], y<-[1..999]]