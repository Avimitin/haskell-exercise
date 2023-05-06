ifEven f x =
  if even x
    then f x
    else x

ifEvenInc x = ifEven (+ 1)

ifEvenDouble x = ifEven (* 2)

ifEvenSquare x = ifEven (^ 2)
