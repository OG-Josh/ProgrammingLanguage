module Interpreter where

import AbsNumbers

eval :: Exp -> Integer
eval (Num n) = n
eval (Plus n m) = (eval n) + (eval m)
eval (Times n m) = (eval n) * (eval m)

eval (Sub n m) = (eval n) - (eval m)
eval (Exponent n m) = (eval n) ^ (eval m)
eval (Divide n m) = (eval n) `div` (eval m)
eval (Modulus n m) = mod (eval n) (eval m)
eval (Negative n) = - (eval n)
eval (Min n m) | (eval n) > (eval m) = (eval m)
                 | (eval n) < (eval m) = (eval n)
eval (Max n m) | (eval n) > (eval m) = (eval n)
                 | (eval n) < (eval m) = (eval m)