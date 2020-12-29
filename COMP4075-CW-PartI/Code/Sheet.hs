module Sheet where

import Data.Array

type CellRef = (Char, Int)

type Sheet a = Array CellRef a

data BinOp = Add | Sub | Mul | Div

data Exp = Lit Double
         | Ref CellRef
         | App BinOp Exp Exp

evalExp :: Sheet Double -> Exp -> Double
evalExp _ (Lit v)        = v
evalExp s (Ref r)        = s ! r
evalExp s (App op e1 e2) = (evalOp op) (evalExp s e1) (evalExp s e2)

evalOp :: BinOp -> (Double -> Double -> Double)
evalOp Add = (+)
evalOp Sub = (-)
evalOp Mul = (*)
evalOp Div = (/)


evalSheet :: Sheet Exp -> Sheet Double
evalSheet s = s'
    where
        s' = array (bounds s) [ (r, evalExp s' (s ! r)) | r <- indices s ]

testSheet :: Sheet Exp
testSheet = array (('a', 1), ('c', 3))
                  [ (('a', 1), Lit 1.0),
                    (('a', 2), Ref ('b', 1)),
                    (('a', 3), Lit 3.0),
                    (('b', 1), App Add (Ref ('c', 2)) (Ref ('b', 2))),
                    (('b', 2), Lit 2.0),
                    (('b', 3), App Mul (Ref ('a', 1)) (Ref ('a', 2))),
                    (('c', 1), App Add (Ref ('a',1)) (Ref ('b',3))),
                    (('c', 2), Lit 3.0),
                    (('c', 3), Lit 7.0)
                  ]
