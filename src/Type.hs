module Type(

) where

data Id = Id Int

data T  = Mu  Id T  | Fun  T' T | Or  T  T  | All    Id T
data T' = Mu' Id T' | Fun' T T' | And T' T' | Exists Id T'

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


dual1 :: Dual a b -> (a -> a) -> (b -> b)
dual1 (to, from) f = to . f . from

dual2 :: Dual a b -> (a -> b) -> (b -> a)
dual2 (to, from) f = from . f . from

dual3 :: Dual a b -> (b -> a) -> (a -> b)
dual3 (to, from) f = to . f . to

dual4 :: Dual a b -> Dual a a -> Dual b b
dual4 (to, from) (f, g) = (to . f . from, to . g . from)
