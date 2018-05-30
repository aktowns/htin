module Builtins.Math where

import           Data.Monoid      ((<>))

import           Builtins.Builtin
import           Types

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
builtinOp x y              = return $ err (pos y) $ "incorrect type given to " <> tshow x <> " builtin. Expected SExpr given " <> tshow y

builtinAddDoc = Just "(+ x y & xs)\nGives the sum of all provided numbers"
builtinAdd :: LVal -> Context LVal
builtinAdd = builtinOp Add

builtinSubDoc = Just "(- x y & xs)\nGives the result of substracting provided numbers in sequence"
builtinSub :: LVal -> Context LVal
builtinSub = builtinOp Sub

builtinMulDoc = Just "(* x y & xs)\nGives the product of all provided numbers"
builtinMul :: LVal -> Context LVal
builtinMul = builtinOp Mul

builtinDivDoc = Just "(/ x y & xs)\nGives the result of dividing the provided numbers in sequence"
builtinDiv :: LVal -> Context LVal
builtinDiv = builtinOp Div

instance Builtin MathBuiltin where
    builtins _ = [ ("+", builtinAddDoc, builtinAdd)
                 , ("-", builtinSubDoc, builtinSub)
                 , ("*", builtinMulDoc, builtinMul)
                 , ("/", builtinDivDoc, builtinDiv)
                 ]
    globals _ = []
    initial _ = return ()
