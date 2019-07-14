module Lambda(
    Exp (Var, App, Lam),
    Id (Id)
) where

import Data.Map as Map
import RegTree

data Id = Id Int deriving(Show)
data Exp = Var Id | App Exp Exp | Lam Id Exp deriving(Show)
