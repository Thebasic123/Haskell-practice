{-# LANGUAGE GADTs #-}

module Solver where

import Formula


-- Evaluating terms
-- ----------------

eval :: Term t -> t
eval (Con v) =  v 
eval (And a b) = (eval a) && (eval b)
eval (Or a b) = (eval a) || (eval b)
eval (Smaller a b) = (eval a) < (eval b)
eval (Plus a b) = (eval a) + (eval b)


-- Checking formulas
-- -----------------

satisfiable :: Formula ts -> Bool
satisfiable (Body a) = eval a
satisfiable (Forall [] _) = False -- if no elements then return false
satisfiable (Forall (x:xs) func) = (satisfiable . func . Con) x || satisfiable (Forall xs func)
-- I found how to use dot opeartor on stack overflow 

solutions :: Formula ts -> [ts]
-- if eval return false, then we don't need to worry current combo
solutions (Body a) = [()| eval a]
-- get all combos by <-, and if final Body is invalid, then we won't print it.
solutions (Forall xs func) = [(x,rest) | x<-xs,rest<-solutions (func (Con x))]