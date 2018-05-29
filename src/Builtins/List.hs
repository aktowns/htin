module Builtins.List where

import           Data.Monoid      ((<>))
import           Data.Text        (Text)
import qualified Data.Text        as T

import           Builtins.Builtin
import           Types

data ListBuiltin = ListBuiltin deriving (Show)

builtinListDoc = Just "(list & items)\nCreates a new list from the items given"
builtinList :: LVal -> Context LVal
builtinList (SExpr xs) = return $ QExpr xs

builtinHeadDoc = Just "(head xs)\nReturns first item in xs, calls list on argument. If xs is nil returns nil."
builtinHead :: LVal -> Context LVal
builtinHead (SExpr xs)
    | (QExpr xs') <- head xs, null xs' = return $ Err "head passed empty list"
    | (QExpr xs') <- head xs           = return $ QExpr [head xs']
    | (Str xs')   <- head xs           = return $ Str $ T.singleton (T.head xs')
    | otherwise                        = return $ Err "head called on non list"

builtinTailDoc = Just "(tail xs)\nReturns possibly empty list of items after the head"
builtinTail :: LVal -> Context LVal
builtinTail (SExpr xs)
    | (QExpr xs') <- head xs, null xs' = return $ Err "tail passed empty list"
    | (QExpr xs') <- head xs           = return $ QExpr $ tail xs'
    | (Str xs')   <- head xs           = return $ Str $ T.tail xs'
    | otherwise                        = return $ Err "tail called on non list"

builtinJoinDoc = Just "(join & xs)\nReturns the concatenation of all supplied lists"
builtinJoin :: LVal -> Context LVal
builtinJoin (SExpr xs)
    | (QExpr _) <- head xs = return $ QExpr $ foldl (\acc (QExpr xs') -> acc ++ xs') [] xs
    | (Str _) <- head xs   = return $ Str $ foldl (\acc (Str xs') -> acc <> xs') T.empty xs
    | otherwise            = return $ Err $ "join called with wrong arguments, got " <> tshow xs

instance Builtin ListBuiltin where
    builtins _ = [ ("list", builtinListDoc, builtinList)
                 , ("join", builtinJoinDoc, builtinJoin)
                 , ("head", builtinHeadDoc, builtinHead)
                 , ("tail", builtinTailDoc, builtinTail)
                 ]
    globals _ = []
    initial _ = return ()
