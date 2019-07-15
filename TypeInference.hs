module TypeInference(

) where

import Lambda
import RegTree
import Data.Map as Map

import Control.Monad.State
import Unique

type FreeVars = Map LId RegTree

functionType :: RegTree -> RegTree -> WithUnique Id RegTree
functionType t1 t2 = do meaninglessId <- unique
                        return (Mu meaninglessId t1 t2)


infer :: Exp -> (RegTree, FreeVars, Substitutions)
infer e = let (res, _) = withUniques (inferI e) (Prelude.map Id [0..])
          in res

inferI :: Exp -> WithUnique Id (RegTree, FreeVars, Substitutions)
inferI (Lambda.Var x) = do t <- unique
                           let var = RegTree.Var t
                           return (var, Map.singleton x var, [])
inferI (Lam x e) = do (eType, free, subs) <- inferI e
                      let free' = delete x free
                      xType <- case Map.lookup x free of
                        Just t -> return t
                        Nothing -> do i <- unique
                                      return (RegTree.Var i)
                      funType <- functionType xType eType
                      return (funType, free', subs)
inferI (App e1 e2) = do (e1Type, free1, subs1) <- inferI e1
                        (e2Type, free2, subs2) <- inferI e2

                        let (free, subs3) = intersectFreeVars free1 free2
                        let subs = subs1 ++ subs2 ++ subs3

                        let e1Type' = applySubs subs e1Type
                        let e2Type' = applySubs subs e2Type
                        let free' = Map.map (applySubs subs) free

                        i <- unique
                        let tau = RegTree.Var i
                        funType <- functionType e2Type' tau
                        let (_, subs4) = intersect e1Type' funType

                        let free'' = Map.map (applySubs subs4) free'
                        let tau' = applySubs subs4 tau

                        return (tau', free'', subs ++ subs4)

-- this function is untested
intersectFreeVars :: FreeVars -> FreeVars -> (FreeVars, Substitutions)
intersectFreeVars free1 free2 =
  let only1 = difference free1 free2
      only2 = difference free2 free1
      both = intersectionWith (\t1 t2 -> (t1, t2)) only1 only2 :: Map LId (RegTree, RegTree)
      (bothIds, bothTypes) = unzip (toList both :: [(LId, (RegTree, RegTree))])

      (intersectedTypes, subs) = intersectList bothTypes
      combined = zip bothIds intersectedTypes

      only1Subbed = Map.map (applySubs subs) only1
      only2Subbed = Map.map (applySubs subs) only2
      bothCombined = fromList combined

      result = unions [only1Subbed, only2Subbed, bothCombined]

      -- intersect each thing in both, and apply substitutions to all subsequent in both as you go.
      -- then, apply all subs to only1 and only2, merge the three maps and return that.
      -- consider monad to do the substitutions automatically as you go.
  in (result, subs)

-- This function is untested
intersectList :: [(RegTree, RegTree)] -> ([RegTree], Substitutions)
intersectList [] = ([], [])
intersectList ((t1,t2) :  ts) = let (t, subs) = intersect t1 t2
                                    (rest, subs2) = intersectList ts
                                    subbedRest = Prelude.map (applySubs subs) rest
                                in (t : subbedRest, subs ++ subs2)


-- Instead of just storing RegTree, store Substitutions -> RegTree
-- and then always evaluate with the current substitutions.

newtype WithSubs = WithSubs (Substitutions -> RegTree)

-- operate inside of a (State Substitutions) monad, and use get to
getType :: WithSubs -> State Substitutions RegTree
getType (WithSubs withSubs) = do subs <- get
                                 return (withSubs subs)
