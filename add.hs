add :: Int -> Int -> Int
add x y  = x + y


mean_value :: Fractional a => a -> a -> a
mean_value x y = (x + y) / 2

max :: Ord a => a -> a -> a
max x y = if x >= y then x else y