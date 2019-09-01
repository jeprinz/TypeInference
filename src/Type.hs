module Type(

) where

import Data.Set as Set
import Unique

data Id = Id Int deriving (Show, Eq, Ord)

data T  = Mu  Id T  | Fun  T' T | Or  T  T  | All    Id T  | Var  Id   deriving (Show)
data T' = Mu' Id T' | Fun' T T' | And T' T' | Exists Id T' | Var' Id deriving (Show)
-- The Id from a (Mu Id T) can only appear in Var, not Var', and can't be directly under Mu.

replace :: T -> Id -> T -> T -- in arg1, replace all id with arg3
replace t var with = doToAllSubs (\i -> if i == var then with else Var i) Var' t

doToAllSubs :: (Id -> T) -> (Id -> T') -> T -> T
doToAllSubs f g (Mu i t) = Mu i (doToAllSubs f g t)
doToAllSubs f g (Fun t' t) = Fun (doToAllSubs' f g t') (doToAllSubs f g t)
doToAllSubs f g (Or t1 t2) = Or (doToAllSubs f g t1) (doToAllSubs f g t2)
doToAllSubs f g (All i t) = All i (doToAllSubs f g t)
doToAllSubs f g (Var i) = f i

doToAllSubs' :: (Id -> T) -> (Id -> T') -> T' -> T'
doToAllSubs' f g (Mu' i t') = Mu' i (doToAllSubs' f g t')
doToAllSubs' f g (Fun' t t') = Fun' (doToAllSubs f g t) (doToAllSubs' f g t')
doToAllSubs' f g (And t1' t2') = And (doToAllSubs' f g t1') (doToAllSubs' f g t2')
doToAllSubs' f g (Exists i t') = Exists i (doToAllSubs' f g t')
doToAllSubs' f g (Var' i) = g i

replace2 :: T -> Id -> T' -> T
replace2 t' var with = doToAllSubs Var (\i -> if i == var then with else Var' i) t'

replace' :: T' -> Id -> T' -> T'
replace' t' i = dual1 tDualT' ((dual5 tDualT' replace) t' i)

type Subs a = [(Id, a)]
type SoFar = Set (Id, Id) -- should be (Subs T, Subs T') so can keep track of all already done?

combineI :: T -> T' -> SoFar -> WithUnique Id (Subs T, Subs T')
combineI (Var i) t' soFar = return ([], [(i, t')])
combineI t (Var' i) soFar = combineI' t (Var' i) soFar
combineI (Mu i t) (Mu' i' t') soFar = if member (i, i') soFar then return ([],[]) else
  combineI (replace t i (Mu i t)) (replace' t' i' (Mu' i' t')) (insert (i, i') soFar)
combineI (Mu i t) t' soFar = combineI (replace t i (Mu i t)) t' soFar
combineI t (Mu' i t') soFar = combineI' t (Mu' i t') soFar
combineI t (And t1' t2') soFar = do (subs1, subs1') <- combineI t t1' soFar
                                    (subs2, subs2') <- combineI t t2' soFar
                                    return (subs1 ++ subs2, subs1' ++ subs2')
combineI (Or t1 t2) t' soFar = combineI' (Or t1 t2) t' soFar
combineI (All i t) t' soFar = do newI <- unique
                                 let newT = replace2 (replace t i (Var newI)) i (Var' newI)
                                 combineI newT t' soFar
combineI t (Exists i t') soFar = combineI' t (Exists i t') soFar


combineI' :: T -> T' -> SoFar -> WithUnique Id (Subs T, Subs T')
combineI' t t' soFar = dual5 t'DualT ((dual5 tDualT' combineI) t') t soFar
--------------------------------------------------------------------------------

tDualT' :: Dual T T'
tDualT' = (to, from) where
  to (Mu i t ) = Mu' i (to t)
  to (Fun t' t) = Fun' (from t') (to t)
  to (Or t1 t2) = And (to t1) (to t2)
  to (All i t) = Exists i (to t)

  from (Mu' i t') = Mu i (from t')
  from (Fun' t t') = Fun (to t) (from t')
  from (And t1 t2) = Or (from t1) (from t2)
  from (Exists i t') = All i (from t')

t'DualT :: Dual T' T
t'DualT = sym tDualT'

tFlip :: Dual T T
tFlip = (flip, flip) where
  (to, from) = tDualT'
  (flip', _) = tFlip'

  flip (Fun t' t) = Fun (flip' (to t)) (flip (from t'))
  flip x = x

tFlip' :: Dual T' T'
tFlip' = dual4 tDualT' tFlip

--------------------------------------------------------------------------------

-- in a greater language, would be able to do:  -- note not even Agda. needs univalence.
-- dual :: Dual a b -> t a b -> t b a
-- instead, settle for some specific cases:
-- proof that b is a dual of a
type Dual a b = (a -> b, b -> a) -- inverses

sym :: Dual a b -> Dual b a
sym (to, from) = (from , to)

dual1 :: Dual a b -> (a -> a) -> (b -> b)
dual1 (to, from) f = to . f . from

dual2 :: Dual a b -> (a -> b) -> (b -> a)
dual2 (to, from) f = from . f . from

dual3 :: Dual a b -> (b -> a) -> (a -> b)
dual3 (to, from) f = to . f . to

dual4 :: Dual a b -> Dual a a -> Dual b b
dual4 (to, from) (f, g) = (to . f . from, to . g . from)

dual5 :: Dual a b -> (a -> c) -> (b -> c)
dual5 (to, from) f = f . from
