type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 src _ dst = [(src, dst)]
hanoi diskAmount src aux dst =
  hanoi (diskAmount - 1) src dst aux
    ++ [(src, dst)]
    ++ hanoi (diskAmount - 1) aux src dst