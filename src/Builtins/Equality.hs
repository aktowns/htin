module Builtins.Equality where

import           Builtins.Builtin
import           Builtins.Guards
import           Types

data EqualityBuiltin = EqualityBuiltin deriving (Show)

-- Eq
builtinEqDoc = Just "(== x y & xs)\nReturns #t if all arguments have the equivalent value"
builtinEq :: LVal -> Context LVal
builtinEq = checked (properCC *> minParams 2 *> args >^ fn)
    where fn pos xs = Boolean pos (all (== head xs) (tail xs))

builtinNeDoc = Just "(!= x y & xs)\nReturns #t if the arguments do not have the equivalent value"
builtinNe :: LVal -> Context LVal
builtinNe = checked (properCC *> minParams 2 *> args >^ fn)
    where fn pos xs = Boolean pos (not (all (== head xs) (tail xs)))

-- Ord
builtinGtDoc = Just "(> x y & xs)\nReturns #t if values are in monotonically increasing order"
builtinGt :: LVal -> Context LVal
builtinGt = checked (properCC *> minParams 2 *> args >^ fn)
    where fn pos xs = Boolean pos (and $ zipWith (>) xs $ drop 1 xs)

builtinLtDoc = Just "(< x y & xs)\nReturns #t if values are in monotonically decreasing order"
builtinLt :: LVal -> Context LVal
builtinLt = checked (properCC *> minParams 2 *> args >^ fn)
    where fn pos xs = Boolean pos (and $ zipWith (<) xs $ drop 1 xs)

builtinGeDoc = Just "(>= x y & xs)\nReturns #t if values are in monotonically non-increasing order"
builtinGe :: LVal -> Context LVal
builtinGe = checked (properCC *> minParams 2 *> args >^ fn)
    where fn pos xs = Boolean pos (and $ zipWith (>=) xs $ drop 1 xs)

builtinLeDoc = Just "(<= x y & xs)\nReturns #t if values are in monotonically non-increasing order"
builtinLe :: LVal -> Context LVal
builtinLe = checked (properCC *> minParams 2 *> args >^ fn)
    where fn pos xs = Boolean pos (and $ zipWith (<=) xs $ drop 1 xs)

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

