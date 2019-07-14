module LambdaParse(

)where

import Lambda
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- This is adaped from the following tutorial:
-- https://wiki.haskell.org/Parsing_a_simple_imperative_language

data Exp = App Exp Exp | Lambda String Exp | Var String deriving(Show)

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "l" ]
           , Token.reservedOpNames = [ "." ] -- TODO: what is difference between reservedNames and reservedOpNames
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
-- integer    = Token.integer    lexer -- parses an integer
whiteSpace = Token.whiteSpace lexer -- parses whitespace

whileParser :: Parser Exp -- the main parser which calls all other ones I think
whileParser = whiteSpace >> expression

expression :: Parser Exp
expression = seqOfExpression

seqOfExpression =
  do list <- (sepBy1 expression' whiteSpace)
     -- If there's only one statement return it without using Seq.
     return $ if length list == 1 then head list else foldl1 App list

expression' :: Parser Exp
expression' = (parens expression) <|> varExp <|> lamExp
-- expression :: Parser Exp
-- expression =   parens expression
--           <|> seqOfExpression
--
-- seqOfExpression =
--   do list <- (sepBy1 expression' whiteSpace)
--      -- If there's only one statement return it without using Seq.
--      return $ if length list == 1 then head list else foldl1 App list
--
-- expression' :: Parser Exp
-- expression' = varExp <|> lamExp

varExp :: Parser Exp
varExp = do varName <- identifier
            return (Var varName)

lamExp :: Parser Exp
lamExp = do reserved "l"
            varName <- identifier
            reservedOp "."
            e <- expression
            return (Lambda varName e)

appExp :: Parser Exp
appExp = do e1 <- expression
            e2 <- expression
            return (App e1 e2)

parseString :: String -> Exp
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r
