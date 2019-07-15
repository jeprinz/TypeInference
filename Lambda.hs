module Lambda(
    Exp (Var, App, Lam),
    LId (LId)
) where

import Data.Map as Map
import RegTree

data LId = LId Int deriving(Show, Eq, Ord)
data Exp = Var LId | App Exp Exp | Lam LId Exp deriving(Show)
