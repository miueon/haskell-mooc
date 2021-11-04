toDistinct xs = foldr f [] xs
    where f x ys = if x `elem` ys then ys else x : ys 