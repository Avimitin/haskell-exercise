isInFirstHalf x list =
    x `elem` take halfListLen list
  where halfListLen = length list `div` 2
