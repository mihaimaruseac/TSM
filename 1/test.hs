{-
sort [] = []
sort (x:xs) = lesser ++ x:greater
  where
    lesser = sort [a | a <- xs, a < x]
    greater = sort [a | a <- xs, a >= x]

fold_r f e [] = e
fold_r f e (x:xs) = f x (fold_r f e xs)

fold_l f e [] = e
fold_l f e (x:xs) = fold_l f (f e x) xs
-}

applyPerm :: [Int] -> [Int] -> [Int]
applyPerm l p = map (\i -> l !! (i - 1)) p

computeOrder :: [Int] -> Int
computeOrder perm = fst $ until test f $ f init
  where
    l = length perm
    test p = snd p == [1 .. l]
    f (a, list) = (a+1, applyPerm list perm)
    init = (0, [1 .. l])
