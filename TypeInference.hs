module TypeInference(

) where

import Lambda
import RegTree

type FreeVars = Map Id RegTree

intersect :: FreeVars -> FreeVars -> FreeVars
intersect = undefined
