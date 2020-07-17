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

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp
  = undefined

showExp :: Exp -> String
showExp
  = undefined

eval :: Exp -> Env -> Double
eval
  = undefined

diff :: Exp -> String -> Exp
diff
  = undefined

maclaurin :: Exp -> Double -> Int -> Double
maclaurin
  = undefined

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

data Code = Lf Int | Nd
  deriving (Show)
data Tree = Node Tree Tree | Leaf Int
  deriving (Show)

dec :: [Code] -> Tree
dec cds = t
  where
    (t, []) = decAux cds
    
decAux :: [Code] -> (Tree, [Code])
decAux ((Lf i):cds) =
      ( Leaf i, cds )
decAux (Nd:cds) = 
     ( Node t1 t2, cds2)
     where
       (t1, cds1) = decAux cds
       (t2, cds2) = decAux cds1

encd :: Tree -> [Code]
encd (Leaf i) = [ Lf i ]
encd (Node t1 t2) = (encd t1)++(encd t2)++ [Nd]

decd :: [Code] -> Tree
decd cds = decdAux cds []

decdAux :: [Code] -> [Tree] -> Tree
decdAux [] (t:ts) = t
decdAux ((Lf i):cds) ts = decdAux cds ((Leaf i):ts)
decdAux (Nd:cds) (t1:t2:ts) = decdAux cds ((Node t2 t1):ts)


data BT = Box | Nde BT BT
  deriving (Eq, Show)

rev :: BT -> BT
rev Box = Box
rev (Nde t1 t2) = Nde (rev t2) (rev t1)

rev1 :: [ (BT,BT) ] -> Bool
rev1 [] = True
rev1 ((bt1, bt2) : bts)
  | bt1 == rev bt2 = rev1 bts
  | otherwise      = False
  and(map (==) (map rev (map head k)) (map 