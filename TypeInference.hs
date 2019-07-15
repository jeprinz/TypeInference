module TypeInference(

) where

import Lambda
import RegTree
import Data.Map as Map

type FreeVars = Map Id RegTree

-- intersectFreeVars :: FreeVars -> FreeVars -> (FreeVars, Substitutions)
-- intersectFreeVars
