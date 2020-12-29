-- 4
data Ivl = Ivl Double Double deriving (Show, Eq)

instance Num Ivl where
    (Ivl lx ux) + (Ivl ly uy) = Ivl (lx + ly) (ux + uy)
    (Ivl lx ux) - (Ivl ly uy) = Ivl (lx - uy) (ux - ly)
    (Ivl lx ux) * (Ivl ly uy) = let vals = [lx * ly, lx * uy, ux * ly, ux * uy]
                                in Ivl (minimum vals) (maximum vals)
    abs (Ivl l u)
        | l >= 0 = Ivl l u
        | u <= 0 = Ivl (-u) (-l)
        | otherwise = Ivl 0.0 (max (abs l) (abs u))

    signum (Ivl l u) = Ivl (signum l) (signum u)

    fromInteger n = Ivl (fromInteger n) (fromInteger n)

instance Fractional Ivl where
    (Ivl lx ux) / (Ivl ly uy)
        | ly == 0 || uy == 0 = error "division by zero"
        | otherwise = let vals = [lx / ly, lx / uy, ux / ly, ux / uy]
                      in Ivl (minimum vals) (maximum vals)
    
    recip (Ivl l u)
        | l == 0 || u == 0 = error "division by zero"
        | otherwise = if signum l == signum u then Ivl (recip u) (recip l)
                                              else Ivl (recip l) (recip u)

    fromRational n = Ivl (fromRational n) (fromRational n)

(+/-) :: Double -> Double -> Ivl
x +/- y = Ivl (x - y) (x + y)

testIvl = t1 && t2 && t3
    where
        t1 = 1 +/- 0.5 == (Ivl 0.5 1.5)
        t2 = Ivl 0 0 + Ivl 1 2 == Ivl 1 2
        t3 = Ivl 2 3 - Ivl (-1) 4 == Ivl (-2.0) 4
