module Derivative where

data Val = Plus Val Val | Minus Val Val | Multiply Val Val | Divide Val Val | NaturalLog Val | ETo Val Val | VTo Val Val | Num Double | X

instance Show Val where
  show (Plus a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
  show (Minus a b) = "(" ++ show a ++ "-" ++ show b  ++ ")"
  show (Multiply a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
  show (Divide a b) = "(" ++ show a ++ "/" ++ show b ++ ")"
  show (NaturalLog a) = "ln (" ++ show a ++ ")"
  show (ETo k a) = show k ++ "e^" ++ show a
  show (VTo x a) = show x ++ "^" ++ show a
  show (Num c) = show c
  show X = "x"

derivative::Val->Val
derivative (Plus a b) = Plus (derivative a) (derivative b)

derivative (Minus a b) = Minus (derivative a) (derivative b)

derivative (Multiply c@(Num _) b) = Multiply c (derivative b)

derivative (Multiply a b) = Plus x y where x = Multiply (derivative a) b
                                           y = Multiply a (derivative b)

derivative (Divide a b) = Divide x y where x = Minus (Multiply (derivative a) b) (Multiply a (derivative b))
                                           y = VTo b (Num 2.0)
derivative (NaturalLog a) = Multiply (Divide (Num 1.0) a) (derivative a)

derivative (ETo k x) = Multiply ( Multiply k (ETo (Num 1.0) x) ) (derivative x)

derivative (VTo x (Num c)) = Multiply (Num c) (VTo x (Num (c - 1)))

derivative (VTo x a) = Multiply (Multiply a (VTo x (Minus a (Num 1.0)))) (derivative x)

derivative (Num _) = Num 0.0

derivative X = Num 1.0

distribute::Double->Val->Val
distribute n (Num a) = Num (a * n)
distribute n X = Multiply (Num n ) X
distribute n (Plus a b) = Plus (distribute n a) (distribute n b)
distribute n (Minus a b) = Minus (distribute n a) (distribute n b)
distribute n (Divide a b) = Divide (distribute n a) b
distribute n (Multiply (Num a) x) = distribute (n*a) x
distribute n (Multiply x (Num a)) = distribute (n*a) x
distribute n a@(NaturalLog _) = Multiply (Num n) a
distribute n e@(ETo _ _) = Multiply (Num n) e
distribute n (VTo x a) = VTo (distribute n x) a

simplify::Val->Val
simplify (Plus (Num a) (Num b)) = Num (a + b)

simplify (Minus (Num a) (Num b)) = Num (a - b)

simplify (Multiply (Num a) (Num b)) = Num (a * b)
simplify (Multiply (Num a) x) = distribute a x
simplify (Multiply x (Num a)) = distribute a x

simplify (Divide (Num a) (Num b)) = Num (a / b)

simplify x = x
