module Test where

import Type
import Unique

uniques = map Id [100..]

combineIt :: T -> T' -> (Subs T, Subs T')
combineIt t t' = fst $ withUniques (combine t t') uniques

-- some variables to use just to make it easier
x = Id 0
y = Id 1
z = Id 2
a = Id 3
b = Id 4
c = Id 5

-- l x . x : forall x . x -> x
typeId = All x (Fun (Var' x) (Var x))

-- l x . x x : forall a b . (a ^ a -> b) -> b
typeSelfAppArg = And (Var' a) (Fun' (Var a) (Var' b))
typeSelfApp = All a (All b (Fun typeSelfAppArg (Var b)))

-- important test case: combine x -> y     forall x. x -> x
-- somehow infinite loop
