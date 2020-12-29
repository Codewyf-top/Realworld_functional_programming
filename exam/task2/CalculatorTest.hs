module CalculatorTest where

import CalculatorBackend (combine)

import Test.HUnit
import Data.Maybe

calc = fromJust . combine

test_calc1 = TestCase (assertEqual "basic" (calc "1+1") 2)
test_calc2 = TestCase (assertEqual "basic" (calc "2-3") (-1))
test_calc3 = TestCase (assertEqual "basic" (calc "1/2") 0.5)
test_calc4 = TestCase (assertEqual "basic" (calc "123*456") (123*456))

test_calc5 = TestCase (assertEqual "complex" (calc "1.2+3.5*2.7") (1.2+3.5*2.7))
test_calc6 = TestCase (assertEqual "complex" (calc "(-1)+2") (-1+2))
test_calc7 = TestCase (assertEqual "complex" (calc "3.2+4.4*5/3/(6.0+7)") (3.2+4.4*5/3/(6.0+7)))
test_calc8 = TestCase (assertEqual "complex" (calc "1/2/3/4/5/6/7") (1/2/3/4/5/6/7))

all_test = TestList [test_calc1, test_calc2, test_calc3, test_calc4,
                     test_calc5, test_calc6, test_calc7, test_calc8]

main :: IO ()
main =  runTestTTAndExit all_test
