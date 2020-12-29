{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- for deriving
import Data.Monoid
import Data.Semigr{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- for deriving
import Data.Monoid
import Data.Semigroup

type Position = (Double, Double)

newtype Length = Length { getLength :: Double } deriving (Num, Eq, Ord, Show)

newtype Area = Area { getArea :: Double } deriving (Num, Eq, Ord, Show)

data Object = Rectangle {centre :: Position, width :: Length, height :: Length}
            | Circle {centre :: Position, radius :: Length}

data Drawing a = Element a
               | Group [Drawing a]

data Statistics = Statistics {
    avgArea :: Area,
    avgCircumference :: Length,
    maxArea :: Area,
    maxCircumference :: Length
} deriving (Show)

data AccumStats = AccumStats {
    asCount :: Sum Int,
    asSumArea :: Sum Area,
    asSumCircumference :: Sum Length,
    asMaxArea :: Max Area,
    asMaxCircumference :: Max Length
}

instance Bounded Area where
    minBound = 0
    maxBound = Area $ read "Infinity"

instance Bounded Length where
    minBound = 0
    maxBound = Length $ read "Infinity"

instance Semigroup AccumStats where
    (<>) (AccumStats cnt1 a1 c1 ma1 mc1) (AccumStats cnt2 a2 c2 ma2 mc2)
        = AccumStats (cnt1 + cnt2) (a1 + a2) (c1 + c2) (max ma1 ma2) (max mc1 mc2)

instance Monoid AccumStats where
    mempty = AccumStats mempty mempty mempty mempty mempty

instance Foldable Drawing where
    foldr f z (Element obj) = f obj z
    foldr f z (Group []) = z
    foldr f z (Group (x:xs)) = foldr f (foldr f z x) (Group xs)

area :: Object -> Area
area (Rectangle c w h) = Area (getLength w * getLength h)
area (Circle c r) = Area (3.14 * (getLength r) * (getLength r))

circumference :: Object -> Length
circumference (Rectangle c w h) = Length (2 * (getLength w + getLength h))
circumference (Circle c r) = Length (2 * 3.14 * getLength r)

statistics' :: Drawing Object -> Statistics
statistics' drawing = let (AccumStats cnt a c ma mc) = foldMap f drawing
                      in Statistics (Area ((getArea (getSum a)) / (fromIntegral (getSum cnt))))
                                    (Length ((getLength (getSum c)) / (fromIntegral (getSum cnt))))
                                    (getMax ma)
                                    (getMax mc)
    where
        f obj = AccumStats 1 (Sum (area obj)) (Sum (circumference obj)) (Max (area obj)) (Max (circumference obj))

drawing = Group [Group [], Group [Element (Circle (0, 0) 1)], Element (Rectangle (0, 0) 3 4)]oup

type Position = (Double, Double)

newtype Length = Length { getLength :: Double } deriving (Num, Eq, Ord, Show)

newtype Area = Area { getArea :: Double } deriving (Num, Eq, Ord, Show)

data Object = Rectangle {centre :: Position, width :: Length, height :: Length}
            | Circle {centre :: Position, radius :: Length}

data Drawing a = Element a
               | Group [Drawing a]

data Statistics = Statistics {
    avgArea :: Area,
    avgCircumference :: Length,
    maxArea :: Area,
    maxCircumference :: Length
} deriving (Show)

data AccumStats = AccumStats {
    asCount :: Sum Int,
    asSumArea :: Sum Area,
    asSumCircumference :: Sum Length,
    asMaxArea :: Max Area,
    asMaxCircumference :: Max Length
}

instance Bounded Area where
    minBound = 0
    maxBound = Area $ read "Infinity"

instance Bounded Length where
    minBound = 0
    maxBound = Length $ read "Infinity"

instance Semigroup AccumStats where
    (<>) (AccumStats cnt1 a1 c1 ma1 mc1) (AccumStats cnt2 a2 c2 ma2 mc2)
        = AccumStats (cnt1 + cnt2) (a1 + a2) (c1 + c2) (max ma1 ma2) (max mc1 mc2)

instance Monoid AccumStats where
    mempty = AccumStats mempty mempty mempty mempty mempty

instance Foldable Drawing where
    foldr f z (Element obj) = f obj z
    foldr f z (Group []) = z
    foldr f z (Group (x:xs)) = foldr f (foldr f z x) (Group xs)

area :: Object -> Area
area (Rectangle c w h) = Area (getLength w * getLength h)
area (Circle c r) = Area (3.14 * (getLength r) * (getLength r))

circumference :: Object -> Length
circumference (Rectangle c w h) = Length (2 * (getLength w + getLength h))
circumference (Circle c r) = Length (2 * 3.14 * getLength r)

statistics' :: Drawing Object -> Statistics
statistics' drawing = let (AccumStats cnt a c ma mc) = foldMap f drawing
                      in Statistics (Area ((getArea (getSum a)) / (fromIntegral (getSum cnt))))
                                    (Length ((getLength (getSum c)) / (fromIntegral (getSum cnt))))
                                    (getMax ma)
                                    (getMax mc)
    where
        f obj = AccumStats 1 (Sum (area obj)) (Sum (circumference obj)) (Max (area obj)) (Max (circumference obj))

drawing = Group [Group [], Group [Element (Circle (0, 0) 1)], Element (Rectangle (0, 0) 3 4)]