module Builtins.Equality where

import           Builtins.Builtin
import           Types

data EqualityBuiltin = EqualityBuiltin deriving (Show)

-- Eq
builtinEqDoc = Just "(== x y & xs)\nReturns #t if all arguments have the equivalent value"
builtinEq :: LVal -> Context LVal
builtinEq (SExpr xs) = return $ Boolean (all (== head xs) (tail xs))

builtinNeDoc = Just "(!= x y & xs)\nReturns #t if the arguments do not have the equivalent value"
builtinNe :: LVal -> Context LVal
builtinNe (SExpr xs) = return $ Boolean (not (all (== head xs) (tail xs)))

-- Ord
builtinGtDoc = Just "(> x y & xs)\nReturns #t if values are in monotonically increasing order"
builtinGt :: LVal -> Context LVal
builtinGt (SExpr xs) = return $ Boolean (and $ zipWith (>) xs $ drop 1 xs)

builtinLtDoc = Just "(< x y & xs)\nReturns #t if values are in monotonically decreasing order"
builtinLt :: LVal -> Context LVal
builtinLt (SExpr xs) = return $ Boolean (and $ zipWith (<) xs $ drop 1 xs)

builtinGeDoc = Just "(>= x y & xs)\nReturns #t if values are in monotonically non-increasing order"
builtinGe :: LVal -> Context LVal
builtinGe (SExpr xs) = return $ Boolean (and $ zipWith (>=) xs $ drop 1 xs)

builtinLeDoc = Just "(<= x y & xs)\nReturns #t if values are in monotonically non-increasing order"
builtinLe :: LVal -> Context LVal
builtinLe (SExpr xs) = return $ Boolean (and $ zipWith (<=) xs $ drop 1 xs)

instance Builtin EqualityBuiltin where
    builtins _ = [ ("==", builtinEqDoc, builtinEq)
                 , ("!=", builtinNeDoc, builtinNe)
                 , (">", builtinGtDoc, builtinGt)
                 , ("<", builtinLtDoc, builtinLt)
                 , (">=", builtinGeDoc, builtinGe)
                 , ("<=", builtinLeDoc, builtinLe)
                 ]
    globals _ = []
    initial _ = return ()

