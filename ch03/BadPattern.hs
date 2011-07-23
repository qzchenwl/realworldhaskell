goodExample (x:xs) = x + goodExample xs
goodExample _      = 0
