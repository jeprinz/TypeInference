module Lambda(

) where

import Data.Map as Map
import RegTree

data Id = Id Int
data Exp = Var Id | App Exp Exp | Lam Id Exp

type FreeVars = Map Id RegTree

intersect :: FreeVars -> FreeVars -> FreeVars
intersect = undefined
