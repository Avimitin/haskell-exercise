counter :: Int -> Int
counter x = ( \a -> ( \b -> (\c -> c) b + 1 ) a + 1) x
