module RegTree(

)where

import Data.Set as Set
-- import Data.BiMap as BiMap
import Data.Map as Map

type Id = Int
data RegTree = Mu Id RegTree RegTree | Var Id deriving (Show)

replace :: RegTree -> Id -> RegTree -> RegTree -- replace t v x = t[x/v]
replace (Mu v t1 t2) var x = Mu v (replace t1 var x) (replace t2 var x)
replace (Var v) var x = if v == var then x else Var v

left :: RegTree -> RegTree
left (Mu topVar t1 t2) = case t1 of
                         (Var v) -> if v == topVar then Mu topVar t1 t2 else Var v
                         (Mu leftVar _ _ ) -> replace t1 topVar oldTree where
                                              oldTree = Mu topVar (Var leftVar) t2

right :: RegTree -> RegTree
right (Mu topVar t1 t2) = left (Mu topVar t2 t1)

type Substitutions = Map Id RegTree

intersect :: RegTree -> RegTree -> (RegTree, Substitutions)
intersect = intersectImpl Map.empty



addToMap :: Substitutions -> Id -> Id -> Substitutions
addToMap ss v1 v2 = if v1 < v2
                         then Map.insert v1 (Var v2) ss
                         else Map.insert v2 (Var v1) ss

--               bound variables  tree1      tree2       result   variables that are set to things
intersectImpl :: Substitutions -> RegTree -> RegTree -> (RegTree, Substitutions)
intersectImpl boundVars (Var v1) (Var v2) =
                         case Map.lookup v1 boundVars of
                              Just subst -> undefined
                              Nothing -> case Map.lookup v2 boundVars of
                                              Just subst -> undefined
                                              Nothing -> (Var (min v1 v2), addToMap Map.empty v1 v2)

example = Mu 0 (Var 0) (Var 0)
-- (A -> A) -> (B -> C)
example2 = Mu 0 (Mu 1 (Var 2) (Var 2)) (Mu 3 (Var 4) (Var 5))
-- u A . (B -> A) -> A -- left is u X . B -> (u A . X -> A)
example3 = Mu 0 (Mu 1 (Var 2) (Var 0)) (Var 0)
