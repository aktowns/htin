module Builtins.List where

import           Control.Monad.Except (throwError)
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Builtins.Builtin
import           Types

data ListBuiltin = ListBuiltin deriving (Show)

builtinListDoc = Just "(list & items)\nCreates a new list from the items given"
builtinList :: LVal -> Context LVal
builtinList (SExpr c xs) = return $ QExpr c xs

builtinHeadDoc = Just "(head xs)\nReturns first item in xs, calls list on argument. If xs is nil returns nil."
builtinHead :: LVal -> Context LVal
builtinHead (SExpr c xs)
    | (QExpr p xs') <- head xs, null xs' = return $ err p "head passed empty list"
    | (QExpr p xs') <- head xs           = return $ QExpr p [head xs']
    | (Str p xs')   <- head xs           = return $ Str p $ T.singleton (T.head xs')
    | otherwise                          = return $ err c "head called on non list"

builtinTailDoc = Just "(tail xs)\nReturns possibly empty list of items after the head"
builtinTail :: LVal -> Context LVal
builtinTail (SExpr c xs)
    | (QExpr p xs') <- head xs, null xs' = return $ err p "tail passed empty list"
    | (QExpr p xs') <- head xs           = return $ QExpr p $ tail xs'
    | (Str p xs')   <- head xs           = return $ Str p $ T.tail xs'
    | otherwise                          = return $ err c "tail called on non list"

builtinJoinDoc = Just "(join & xs)\nReturns the concatenation of all supplied lists"
builtinJoin :: LVal -> Context LVal
builtinJoin (SExpr c xs)
    | (QExpr _ _) <- head xs = return $ QExpr c $ foldl (\acc (QExpr _ xs') -> acc <> xs') [] xs
    | (Str p _) <- head xs   =
        if all isStr xs then return $ Str p $ foldl (\acc (Str _ xs') -> acc <> xs') T.empty xs
        else throwError $ RuntimeException (err p $ "join needs either a QExpr of different types or be all strings, was given " <> (tshow xs))
    | otherwise            = return $ err c $ "join called with wrong arguments, got " <> tshow xs
builtinJoin x = error $ "internal error: builtinJoin called with argument " ++ show x

instance Builtin ListBuiltin where
    builtins _ = [ ("list", builtinListDoc, builtinList)
                 , ("join", builtinJoinDoc, builtinJoin)
                 , ("head", builtinHeadDoc, builtinHead)
                 , ("tail", builtinTailDoc, builtinTail)
                 ]
    globals _ = []
    initial _ = return ()
