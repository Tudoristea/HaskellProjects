module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

class Vars a where
  x, y, z :: a

instance Vars Exp where
  x = Id "x"
  y = Id "y"
  z = Id "z"

instance Vars Double where
  x = 4.3
  y = 9.2
  z = -1.7

instance Num Exp where
  fromInteger = undefined
  negate      = undefined
  (+)         = undefined
  (*)         = undefined
-- Leave the following two undefined...
  signum      = undefined
  abs         = undefined

instance Fractional Exp where
  fromRational = undefined
  (/)          = undefined
-- Leave the following one undefined...
  recip        = undefined

instance Floating Exp where
  sin     = undefined
  cos     = undefined
  log     = undefined
-- Leave the following fifteen undefined...
  tan     = undefined
  asin    = undefined
  acos    = undefined
  atan    = undefined
  pi      = undefined
  exp     = undefined
  sqrt    = undefined
  (**)    = undefined
  logBase = undefined
  sinh    = undefined
  cosh    = undefined
  tanh    = undefined
  asinh   = undefined
  acosh   = undefined
  atanh   = undefined

---------------------------------------------------------------------------
tabelUni     = [(Neg, ((-1) * )), (Sin, (sin )), (Cos, (cos )), (Log, (log ))]
tabelBin     = [(Add, ((+) )), (Mul, ((*) )), (Div, ((/) ))]
tabelUniStr  = [(Neg, " -1 * "), (Sin, " sin "), (Cos, " cos "), (Log, " log ")]
tabelBinStr  = [(Add, " + "), (Mul, " * "), (Div, " / ")]

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp k
  = fromJust.lookup k

showExp :: Exp -> String
showExp (Val x)                  = show x
showExp (Id x)                   = show x
showExp (BinApp binOp exp1 exp2) = "(" ++ (showExp exp1) ++ (lookUp binOp tabelBinStr) ++ (showExp exp2) ++ ")"
showExp (UnApp unOp exp)         = (lookUp unOp tabelUniStr) ++ "(" ++ (showExp exp) ++ ")"


eval :: Exp -> Env -> Double
eval (Val x) env                  = x
eval (Id x) env                   = lookUp x env
eval (BinApp binOp exp1 exp2) env = (lookUp binOp tabelBin) (eval exp1 env) (eval  exp2 env)
eval (UnApp unOp exp) env         = (lookUp unOp tabelUni) (eval exp env)

diff :: Exp -> String -> Exp
diff (Val a) x = (Val 0.0)
diff (Id a) x
  | a == x  = (Val 1.0)
  | otherwise = (Val 0.0)
diff (BinApp binOp exp1 exp2) x
  | binOp == Mul = BinApp Add (BinApp Mul exp1 dx2) (BinApp Mul dx1 exp2)
  | binOp == Div = BinApp Div (BinApp Add (BinApp Mul dx1 (exp2)) (UnApp Neg (BinApp Mul (exp1) dx2))) (BinApp Mul exp2 exp2)
  | binOp == Add = BinApp Add dx1 dx2
    where
      dx1 = diff exp1 x
      dx2 = diff exp2 x
diff (UnApp unOp exp) x
  | unOp == Neg = UnApp Neg dx
  | unOp == Sin = BinApp Mul (UnApp Cos exp) dx
  | unOp == Cos = UnApp Neg (BinApp Mul (UnApp Sin exp) dx)
  | unOp == Log = BinApp Div dx exp -- Normal way:  | unOp == Log = BinApp Mul (BinApp Div (Val 1)  exp) dx
    where
      dx  = diff exp x

maclaurin :: Exp -> Double -> Int -> Double
maclaurin exp a n
  = maclaurin' exp 0
    where 
      maclaurin' :: Exp -> Int -> Double
      maclaurin' exp k
        | k >= n    = 0.0
        | otherwise = (eval exp [("x", 0.0)]) * (a ^ (fromIntegral k)) / fromIntegral (product [1..k]) + maclaurin' (diff exp "x") (k + 1)

        
maclaurin2 :: Exp -> Double -> Int -> Double
maclaurin2 exp a n
  = sum (zipWith3 macFct fact differ power)
    where
      fact   = scanl (*) 1 [1..n-1]
      differ = map (flip eval  [("x", 0.0)]) (iterate (flip diff "x") exp)
      power  = iterate (a * ) 1
      macFct :: Int -> Double -> Double -> Double
      macFct fact' differ' power'
        = power' * differ' / fromIntegral (fact')



---------------------------------------------------------------------------
-- Test cases...

e1, e2, e3, e4, e5, e6 :: Exp

-- 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- x*x+y-7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))
