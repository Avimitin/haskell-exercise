myGCD x 0 = x
myGCD a b = myGCD b (a `mod` b)
