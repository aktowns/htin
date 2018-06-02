{-# LANGUAGE QuasiQuotes #-}
module Builtins.Equality(EqualityBuiltin(..)) where

import           Data.Text        (Text)

import           Builtins.Builtin
import           Builtins.Guards
import           Types
import           Utils.Doc

data EqualityBuiltin = EqualityBuiltin deriving (Show)

-- Eq
[genDoc|eqEq
(eq/== x y & xs)

Returns #t if all arguments have the equivalent value
|]
eqEq :: LVal -> Context LVal
eqEq = checked $ (properCC *> minParams 2 *> args) >^
    \p xs -> Boolean p (all (== head xs) (tail xs))

[genDoc|eqNe
(eq/!= x y & xs)

Returns #t if the arguments do not have the equivalent value
|]
eqNe :: LVal -> Context LVal
eqNe = checked $ (properCC *> minParams 2 *> args) >^
    \p xs -> Boolean p (not (all (== head xs) (tail xs)))

-- Ord
[genDoc|ordGt
(ord/> x y & xs)

Returns #t if values are in monotonically increasing order
|]
ordGt :: LVal -> Context LVal
ordGt = checked $ (properCC *> minParams 2 *> args) >^
    \p xs -> Boolean p (and $ zipWith (>) xs $ drop 1 xs)

[genDoc|ordLt
(ord/< x y & xs)

Returns #t if values are in monotonically decreasing order
|]
ordLt :: LVal -> Context LVal
ordLt = checked $ (properCC *> minParams 2 *> args) >^
    \p xs -> Boolean p (and $ zipWith (<) xs $ drop 1 xs)

[genDoc|ordGe
(ord/>= x y & xs)

Returns #t if values are in monotonically non-increasing order
|]
ordGe :: LVal -> Context LVal
ordGe = checked $ (properCC *> minParams 2 *> args) >^
    \p xs -> Boolean p (and $ zipWith (>=) xs $ drop 1 xs)

[genDoc|ordLe
(ord/<= x y & xs)

Returns #t if values are in monotonically non-decreasing order
|]
ordLe :: LVal -> Context LVal
ordLe = checked $ (properCC *> minParams 2 *> args) >^
    \p xs -> Boolean p (and $ zipWith (<=) xs $ drop 1 xs)

instance Builtin EqualityBuiltin where
    builtins _ = [ ("eq/==",  eqEqDoc,  eqEq)
                 , ("eq/!=",  eqNeDoc,  eqNe)
                 , ("ord/>",  ordGtDoc, ordGt)
                 , ("ord/<",  ordLtDoc, ordLt)
                 , ("ord/>=", ordGeDoc, ordGe)
                 , ("ord/<=", ordLeDoc, ordLe)
                 ]
    globals _ = []
    initial _ = return ()
