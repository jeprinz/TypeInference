module Main where

import LambdaParse
import TypeInference
import RegTree
import Data.Map as Map
import Lambda
import Data.List as List

main :: IO ()
main = do takeLine
          main

takeLine :: IO ()
takeLine = do putStr ">> "
              line <- getLine
              let (e, varNames) = lambdaParseInfo line
              let (t, free, _) = infer e
              let (lIds, types) = unzip (toList free)
              let (asString: freeVarTypes) = typesToStrings (t: types)
              putStrLn asString
              putStrLn (freeVarInfo lIds varNames freeVarTypes)
-- main = putStrLn "Hello, Haskell!"

freeVarInfo :: [LId] -> (Map LId String) -> [String] -> String
freeVarInfo lIds nameMap types =
          let maybenames = Prelude.map ((flip Map.lookup) nameMap) lIds
              names = Prelude.map (\mn -> case mn of
                        Just s -> s
                        Nothing -> "Error") maybenames
              namesTypes = zip names types :: [(String, String)]
              lines = Prelude.map (\(var, t) -> var ++ ": " ++ t) namesTypes
          in intercalate "\n" lines
