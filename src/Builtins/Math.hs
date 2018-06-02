{-# LANGUAGE QuasiQuotes #-}
module Builtins.Math(MathBuiltin(..)) where

import           Data.Monoid      ((<>))
import           Data.Text        (Text)
import           Formatting

import           Builtins.Builtin
import           Types
import           Utils.Doc

data MathBuiltin = MathBuiltin deriving (Show)

data Op = Add | Sub | Mul | Div deriving (Show)

lvalNum :: LVal -> Integer
lvalNum (Num _ x) = x
lvalNum x         = error $ "internal error: num was expected but given: " <> show x

builtinOp :: Op -> LVal -> Context LVal
builtinOp Add (SExpr c xs) = return $ Num c $ sum $ map lvalNum xs
builtinOp Sub (SExpr c xs) = return $ Num c $ foldl1 (-) $ map lvalNum xs
builtinOp Mul (SExpr c xs) = return $ Num c $ product $ map lvalNum xs
builtinOp Div (SExpr c xs) = return $ Num c $ foldl1 quot $ map lvalNum xs
builtinOp x y              = return $ err (pos y) ("incorrect type given to " % shown % " builtin. Expected SExpr given " % lval) x y

[genDoc|mathAdd
(math/+ x y & xs)

Gives the sum of all provided numbers
|]
mathAdd :: LVal -> Context LVal
mathAdd = builtinOp Add

[genDoc|mathSub
(math/- x y & xs)

Gives the result of substracting provided numbers in sequence
|]
mathSub :: LVal -> Context LVal
mathSub = builtinOp Sub

[genDoc|mathMul
(math/* x y & xs)

Gives the product of all provided numbers
|]
mathMul :: LVal -> Context LVal
mathMul = builtinOp Mul

[genDoc|mathDiv
(math// x y & xs)

Gives the result of dividing the provided numbers in sequence
|]
mathDiv :: LVal -> Context LVal
mathDiv = builtinOp Div

instance Builtin MathBuiltin where
    builtins _ = [ ("math/+", mathAddDoc, mathAdd)
                 , ("math/-", mathSubDoc, mathSub)
                 , ("math/*", mathMulDoc, mathMul)
                 , ("math//", mathDivDoc, mathDiv)
                 ]
    globals _ = []
    initial _ = return ()
