module Main where

import LambdaParse
import TypeInference
import RegTree

main :: IO ()
main = do putStr ">> "
          line <- getLine
          let e = lambdaParse line
          let (t, _, _) = infer e
          let asString = typeToString t
          putStrLn asString
-- main = putStrLn "Hello, Haskell!"
