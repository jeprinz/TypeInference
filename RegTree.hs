module RegTree(
    RegTree,
    intersect,
    replace,
    typeToString,
    freeVars
)where

import Data.Set as Set
-- import Data.BiMap as BiMap
import Data.Map as Map
import Control.Monad.State

data Id = Id Int | Idx Id Id deriving (Show, Eq, Ord)
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

name :: RegTree -> Id
name (Var v) = v
name (Mu v _ _) = v

-- TODO: make this into List (Id, RegTree)
type Substitutions = Map Id RegTree

intersectI :: Set Id -> RegTree -> RegTree -> (RegTree, Substitutions)
intersectI b t1 t2 = let x = name t1
                         y = name t2
                     in if Set.member (Idx x y) b then (Var (Idx x y), Map.empty)
                        else case (t1, t2) of
                             ((Mu _ _ _), (Mu _ _ _)) -> (Mu (Idx x y) t1' t2', Map.union b1 b2) where
                                 b' = Set.insert (Idx x y) b
                                 (t1', b1) = intersectI b' (left t1) (left t2)
                                 (t2', b2) = intersectI b' (right t1) (right t2)
                             ((Mu _ _ _), (Var y)) -> (t1, Map.insert y t1 Map.empty)
                             ((Var x), (Mu _ _ _)) -> (t2, Map.insert x t2 Map.empty)
                             ((Var x), (Var y)) -> (t1, Map.insert y t1 Map.empty)

intersect :: RegTree -> RegTree -> (RegTree, Substitutions)
intersect = intersectI Set.empty

freeVarsI :: RegTree -> Set Id -> Set Id
freeVarsI (Var v) bound = if Set.member v bound then Set.empty else Set.singleton v
freeVarsI (Mu v t1 t2) bound = Set.union (freeVarsI t1 bound') (freeVarsI t2 bound') where
    bound' = Set.insert v bound

freeVars :: RegTree -> Set Id
freeVars t = freeVarsI t Set.empty

example = Mu (Id 0) (Var (Id 0)) (Var (Id 0))
-- (A -> A) -> (B -> C)
example2 = Mu (Id 0) (Mu (Id 1) (Var (Id 2)) (Var (Id 2))) (Mu (Id 3) (Var (Id 4)) (Var (Id 5)))
-- u A . (B -> A) -> A -- left is u X . B -> (u A . X -> A)
example3 = Mu (Id 0) (Mu (Id 1) (Var (Id 2)) (Var (Id 0))) (Var (Id 0))


-- everthing below here is for converting types to strings
type TypeState = State (String, Map Id Char)

typeToStringI :: RegTree -> Bool -> TypeState String
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

typeToString :: RegTree -> String
typeToString t = evalState (typeToStringI t False) (['A'..'Z'], Map.empty)

getName :: Id -> TypeState Char
getName v = do vars <- getMap
               case Map.lookup v vars of
                    Just name -> return name
                    Nothing -> do u <- unique
                                  putMap (Map.insert v u vars)
                                  return u

unique :: TypeState Char
unique = do (us, vars) <- get
            put (tail us, vars)
            return (head us)

getMap :: TypeState (Map Id Char)
getMap = do (us, vars) <- get
            return vars

putMap :: Map Id Char -> TypeState ()
putMap newMap = do (us, _) <- get
                   put (us, newMap)
                   return ()

    -- case Map.lookup v vars of
    -- Just s -> return s
    -- Nothing -> unique
