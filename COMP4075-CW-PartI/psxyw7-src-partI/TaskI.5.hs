-- 5
type Position = (Double, Double)

type Length = Double

type Area = Double

data Object = Rectangle {centre :: Position, width :: Length, height :: Length}
            | Circle {centre :: Position, radius :: Length}

data Drawing a = Element a
               | Group [Drawing a]

data Statistics = Statistics {
    avgArea :: Area,
    avgCircumference :: Length,
    maxArea :: Area,
    maxCircumference :: Length
} deriving Show

data AccumStats = AccumStats {
    asCount :: Int,
    asSumArea :: Area,
    asSumCircumference :: Length,
    asMaxArea :: Area,
    asMaxCircumference :: Length
}

area :: Object -> Area
area (Rectangle c w h) = w * h
area (Circle c r) = 3.14 * r * r

circumference :: Object -> Length
circumference (Rectangle c w h) = 2 * (w + h)
circumference (Circle c r) = 2 * 3.14 * r

addAccumStats :: AccumStats -> AccumStats -> AccumStats
addAccumStats (AccumStats cnt1 a1 c1 ma1 mc1) (AccumStats cnt2 a2 c2 ma2 mc2)
    = AccumStats (cnt1 + cnt2) (a1 + a2) (c1 + c2) (max ma1 ma2) (max mc1 mc2)

accumStat :: Drawing Object -> AccumStats
accumStat (Element obj) = AccumStats 1 (area obj) (circumference obj) (area obj) (circumference obj)
accumStat (Group []) = AccumStats 0 0 0 0 0
accumStat (Group (x:xs)) = addAccumStats (accumStat x) (accumStat (Group xs))

statistics :: Drawing Object -> Statistics
statistics drawing = let (AccumStats cnt a c ma mc) = accumStat drawing
                     in Statistics (a / (fromIntegral cnt)) (c / (fromIntegral cnt)) ma mc

instance Foldable Drawing where
    foldr f z (Element obj) = f obj z
    foldr f z (Group []) = z
    foldr f z (Group (x:xs)) = foldr f (foldr f z x) (Group xs)

instance Semigroup AccumStats where
    (<>) = addAccumStats

instance Monoid AccumStats where
    mempty = AccumStats 0 0 0 0 0

statistics' :: Drawing Object -> Statistics
statistics' drawing = let (AccumStats cnt a c ma mc) = foldMap f drawing
                      in Statistics (a / (fromIntegral cnt)) (c / (fromIntegral cnt)) ma mc
    where
        f obj = AccumStats 1 (area obj) (circumference obj) (area obj) (circumference obj)


drawing = Group [Group [], Group [Element (Circle (0, 0) 1)], Element (Rectangle (0, 0) 3 4)]