-- 1.1
merge :: Ord a => [a] -> [a] -> [a]
merge xxs@(x:xs) yys@(y:ys)
    | x == y = x : merge xs ys
    | x < y = x : merge xs yys
    | x > y = y : merge xxs ys

hamming :: [Integer]
hamming = 1 : (merge (map (*2) [1..]) $ merge (map (*3) [1..]) (map (*5) [1..]))


