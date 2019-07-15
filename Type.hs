module Type(
    Type,
    Type.product,
    replace,
    typeToString,
    freeVars
)where

import Data.Set as Set
-- import Data.BiMap as BiMap
import Data.Map as Map
import Control.Monad.State
import NameGiver

data Id = Id Int deriving (Show, Eq, Ord)
data Type = Mu Id Type Type | Var Id deriving (Show)


replace :: Type -> Id -> Type -> Type -- replace t v x = t[x/v]
replace (Mu v t1 t2) var x = Mu v (replace t1 var x) (replace t2 var x)
replace (Var v) var x = if v == var then x else Var v

left :: Type -> Type
left (Mu topVar t1 t2) = case t1 of
                         (Var v) -> if v == topVar then Mu topVar t1 t2 else Var v
                         (Mu leftVar _ _ ) -> replace t1 topVar oldTree where
                                              oldTree = Mu topVar (Var leftVar) t2

right :: Type -> Type
right (Mu topVar t1 t2) = left (Mu topVar t2 t1)

name :: Type -> Id
name (Var v) = v
name (Mu v _ _) = v

data ProdType = PMu Id Id ProdType ProdType | PVar Id Id | Leaf Type Type deriving(Show)

productI :: Set (Id, Id) -> Type -> Type -> ProdType
productI b t1 t2 = let x = name t1
                       y = name t2
                    in if Set.member (x, y) b then PVar x y
                       else case (t1, t2) of
                            ((Mu _ _ _), (Mu _ _ _)) -> PMu x y leftSide rightSide where
                                b' = Set.insert (x, y) b
                                leftSide = productI b' (left t1) (left t2)
                                rightSide = productI b' (right t1) (right t2)
                            ((Mu x _ _), (Var y)) -> Leaf t1 t2
                            ((Var x), (Mu y _ _)) -> Leaf t1 t2
                            ((Var x), (Var y)) -> Leaf t1 t2

product :: Type -> Type -> ProdType
product = productI Set.empty

example = Mu (Id 0) (Var (Id 0)) (Var (Id 0))
-- (A -> A) -> (B -> C)
example2 = Mu (Id 0) (Mu (Id 1) (Var (Id 2)) (Var (Id 2))) (Mu (Id 3) (Var (Id 4)) (Var (Id 5)))
-- u A . (B -> A) -> A -- left is u X . B -> (u A . X -> A)
example3 = Mu (Id 0) (Mu (Id 1) (Var (Id 2)) (Var (Id 0))) (Var (Id 0))

example4 = Mu (Id 0) (Mu (Id 1) (Var (Id 3)) (Var (Id 3))) (Mu (Id 2) (Var (Id 4)) (Var (Id 5)))
example5 = Mu (Id 10) (Mu (Id 11) (Var (Id 13)) (Var (Id 14))) (Mu (Id 12) (Var (Id 15)) (Var (Id 16)))
example6 = (Mu (Id 0) (Var (Id 1)) (Var (Id 1)))
example7 = Mu (Id 10) (Var (Id 11)) (Mu (Id 12) (Var (Id 11)) (Var (Id 11)))
example7' = Mu (Id 10) (Var (Id 1)) (Mu (Id 11) (Var (Id 1)) (Var (Id 1)))
example8 = Mu (Id 10) (Mu (Id 11) (Var (Id 1)) (Var (Id 1))) (Mu (Id 12) (Var (Id 1)) (Var (Id 1)))


-- everthing below here is for converting types to strings
type TypeState' = TypeState Id Char

freeVarsI :: Type -> Set Id -> Set Id
freeVarsI (Var v) bound = if Set.member v bound then Set.empty else Set.singleton v
freeVarsI (Mu v t1 t2) bound = Set.union (freeVarsI t1 bound') (freeVarsI t2 bound') where
    bound' = Set.insert v bound

freeVars :: Type -> Set Id
freeVars t = freeVarsI t Set.empty

typeToStringI :: Type -> Bool -> TypeState' String
typeToStringI (Mu v t1 t2) parens = do
    prefix <- if Set.member v (freeVars t1) || Set.member v (freeVars t2)
                 then do var <- getName v
                         -- return ("μ" ++ [var] ++ ".")
                         return ("u" ++ [var] ++ ".")
    else return ""
    s1 <- typeToStringI t1 True
    s2 <- typeToStringI t2 False
    return (if parens then (prefix ++ "(" ++ s1 ++ "->" ++ s2 ++ ")")
                      else (prefix ++ s1 ++ "->" ++ s2))


typeToStringI (Var v) _ = do name <- getName v
                             return [name]

typeToString :: Type -> String
typeToString t = evalState (typeToStringI t False) (['A'..'Z'], Map.empty)
