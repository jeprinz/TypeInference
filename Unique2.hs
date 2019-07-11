module Unique(
) where

import Control.Monad
import Data.List
import Data.Int
import Data.Eq
-- import Data.List.Utils (replace)

replace :: (Eq a) => a -> a -> [a] -> [a]
replace _ _ [] = []
replace x y (a : as) = if a == x then y : as else a : as

newtype WithUnique u a = WithUnique {withUniques :: [u] -> ([u] -> a,[u]) }

instance Functor (WithUnique u) where
  fmap f (WithUnique utoa) = WithUnique
    (\us -> let (a, us') = utoa us in (f . a, us'))

instance Applicative (WithUnique u) where
  pure a = WithUnique (\u -> (const a, u))
  (WithUnique utof) <*> (WithUnique utoa) =
    WithUnique (\us -> let (f, us') = utof us
                           (a, us'') = utoa us'
                       in  (undefined , us''))


instance Monad (WithUnique u) where
  return a = WithUnique (\u -> (a, u))
  (WithUnique utoa) >>= f = WithUnique (\us ->
      let (a, us') = utoa us
          (WithUnique fa) = f a
      in fa us')

uniqueImpl :: [u] -> (u, [u])
uniqueImpl (u : us) = (u, us)

unique :: WithUnique u u
unique = WithUnique uniqueImpl

merge :: (Eq u) => u -> u -> WithUnique u ()
merge u1 u2 = WithUnique (\us -> ((), replace u1 u2 us))

-- runWithUnique : (WithUnique u a) -> [u] -> a
-- runWithUnique wu = let (a us) = withUniques in a

example :: WithUnique Char [Char]
example = do
  a <- unique
  b <- unique
  merge a b
  return ['h', a, a, a, b]
