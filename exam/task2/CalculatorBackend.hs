module CalculatorBackend where

import Data.Char
import Data.Maybe

data Exp = TVar   Double
         | TAdd   Exp Exp
         | TMinus Exp Exp
         | TMult  Exp Exp
         | TDiv   Exp Exp
         deriving (Show)

type Token = String

eval :: Exp -> Double
eval (TVar d) = d
eval (TAdd e1 e2) = eval e1 + eval e2
eval (TMinus e1 e2) = eval e1 - eval e2
eval (TMult e1 e2) = eval e1 * eval e2
eval (TDiv e1 e2) = eval e1 / eval e2

split :: String -> [Token]
split [] = []
split input@(x:xs)
    | isDigit x = takeWhile isDigit input : split (dropWhile isDigit input)
    | x `elem` ['+', '-', '*', '/', '(', ')', '.'] = [x] : split xs

tokenize :: [Token] -> Maybe Token -> Maybe [Token]
tokenize [] Nothing = Just []
tokenize [] (Just x) = Just [x]
tokenize (x:xs) m
    | x == "." = if isNothing m || null xs || not (isNumberStr (head xs))
                    then Nothing
                    else ((fromJust m ++ "." ++ head xs) :) <$> tokenize (tail xs) Nothing
    | isJust m = (fromJust m :) <$> tokenize (x:xs) Nothing
    | isNumberStr x = tokenize xs (Just x)
    | otherwise = (x :) <$> tokenize xs Nothing

isNumberStr :: String -> Bool
isNumberStr = isDigit . head

numberToExp :: String -> Exp
numberToExp s = TVar (read s :: Double)

buildExp :: Exp -> Exp -> String -> Exp
buildExp e1 e2 op = case op of "+" -> TAdd e1 e2
                               "-" -> TMinus e1 e2
                               "*" -> TMult e1 e2
                               "/" -> TDiv e1 e2

setSign :: String -> String -> Exp
setSign "+" e = numberToExp e
setSign "-" e = numberToExp ('-':e)

parse :: [Token] -> [Exp] -> [String] -> Maybe Exp
parse [] [x] [] = Just x
parse [] (x:y:xs) (op:ops) = parse [] (buildExp y x op:xs) ops
parse (t:ts) exps ops
    | t `elem` ["+", "-", "*", "/"] = parse ts exps (t:ops)
    | t == "(" = if length ts > 1 && head ts `elem` ["+", "-"]
                    then parse (tail (tail ts)) (setSign (head ts) (head (tail ts)) : exps) (t:ops)
                    else parse ts exps (t:ops)
    | not (null ops) && head ops == "(" && t == ")"= parse ts exps (tail ops)
    | t == ")" = parse (t:ts) (buildExp (head (tail exps)) (head exps) (head ops) : tail (tail exps)) (tail ops)
    | otherwise = if null ops || head ops `elem` ["+", "-", "("]
                    then parse ts (numberToExp t:exps) ops
                    else parse ts (buildExp (head exps) (numberToExp t) (head ops) : tail exps) (tail ops)
parse _ _ _ = Nothing

combine :: String -> Maybe Double
combine exp = eval <$> (tokenize (split exp) Nothing >>= (\x -> parse x [] []))

prettyDouble :: String -> String
prettyDouble xs = let fracs = tail $ dropWhile (/='.') xs
                  in if all (=='0') fracs then takeWhile (/='.') xs else xs

calc :: String -> String
calc exp = maybe "ERR!" (prettyDouble . show) (combine exp)
