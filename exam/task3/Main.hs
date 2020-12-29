import Test.QuickCheck

import           Prelude hiding (head, tail, lookup, drop)
import qualified Prelude as P (drop)

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show, Eq)
type RList a = [(Int, Tree a)]

empty :: RList a
empty = []

isEmpty :: RList a -> Bool
isEmpty = null

cons :: a -> RList a -> RList a
cons x ((w1, t1) : (w2, t2) : wts) | w1 == w2 =
    (w1 * 2 + 1, Node t1 x t2) : wts
cons x wts = (1, Leaf x) : wts

head :: RList a -> a
head ((_, Leaf x)     : _) = x
head ((_, Node _ x _) : _) = x

tail :: RList a -> RList a
tail ((_, Leaf _): wts)        = wts
tail ((w, Node t1 _ t2) : wts) = (w', t1) : (w', t2) : wts
    where
        w' = w `div` 2

lookup :: Int -> RList a -> a
lookup i ((w, t) : wts) | i < w      = lookupTree i w t
                        | otherwise  = lookup (i - w) wts

lookupTree :: Int -> Int -> Tree a -> a
lookupTree _ _ (Leaf x) = x
lookupTree i w (Node t1 x t2)
    | i == 0    = x
    | i <= w'   = lookupTree (i - 1) w' t1
    | otherwise = lookupTree (i - w' - 1) w' t2
    where
        w' = w `div` 2

update :: Int -> a -> RList a -> RList a
update i x (wt@(w, t) : wts) | i < w      = (w, updateTree i x w t) : wts
                             | otherwise  = wt : update (i - w) x wts

updateTree :: Int -> a -> Int -> Tree a -> Tree a
updateTree _ x _ (Leaf _) = Leaf x
updateTree i x w (Node t1 y t2)
    | i == 0    = Node t1 x t2
    | i <= w'   = Node (updateTree (i - 1) x w' t1) y t2
    | otherwise = Node t1 y (updateTree (i - w' - 1) x w' t2)
    where
        w' = w `div` 2

drop :: Int -> RList a -> RList a
drop _ [] = []
drop 0 t = t
drop n ((w, t) : wts) = if n >= w then drop (n - w) wts else dropTree n w t ++ wts
    where
        dropTree 0 _ (Leaf x) = [(1, Leaf x)] -- drop 0 for leaf
        dropTree _ _ (Leaf _) = []
        dropTree 0 w t = [(w, t)] -- drop 0 for node
        dropTree n w (Node t1 _ t2) = let w' = w `div` 2 in
                                        if n > w' then dropTree (n - w' - 1) w' t2 -- remove left and drop right
                                                  else dropTree (n - 1) w' t1 ++ [(w', t2)] -- drop left and keep right

toRList :: [a] -> RList a
toRList = foldr cons empty

fromRList :: RList a -> [a]
fromRList [] = []
fromRList r = head r : fromRList (tail r)

prop_FromRListToRList :: [Int] -> Property
prop_FromRListToRList xs = label ("length: " ++ show (length xs)) (fromRList (toRList xs) == xs)

prop_Drop :: [Int] -> Property
prop_Drop xs = label ("length: " ++ show (length xs) ++ " drop number: " ++ show drop_length)
                     (P.drop drop_length xs == fromRList (drop drop_length (toRList xs)))
    where
        drop_length = 5

prop_Update :: Int -> NonEmptyList Int -> Property
prop_Update x xs = length l > pos ==> label ("length: " ++ show (length l) ++ "update position: " ++ show pos)
                                            (fromRList (update pos x (toRList l)) == take pos l ++ [x] ++ P.drop (pos + 1) l)
    where
        l = getNonEmpty xs
        pos = 3