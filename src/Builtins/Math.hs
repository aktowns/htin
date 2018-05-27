module Builtins.Math where

import           Builtins.Builtin
import           Types

data MathBuiltin = MathBuiltin deriving (Show)

data Op = Add | Sub | Mul | Div deriving (Show)

lvalNum :: LVal -> Integer
lvalNum (Num x) = x
lvalNum x       = error $ "Num was expected but given: " ++ show x

builtinOp :: Op -> LVal -> Context LVal
builtinOp Add (SExpr xs) = return $ Num $ sum $ map lvalNum xs
builtinOp Sub (SExpr xs) = return $ Num $ foldl1 (-) $ map lvalNum xs
builtinOp Mul (SExpr xs) = return $ Num $ product $ map lvalNum xs
builtinOp Div (SExpr xs) = return $ Num $ foldl1 quot $ map lvalNum xs
builtinOp x y = error $ "Incorrect type given to " ++ show x ++ " builtin. Expected SExpr given " ++ show y


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
