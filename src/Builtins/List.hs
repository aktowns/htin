{-# LANGUAGE QuasiQuotes #-}
module Builtins.List(ListBuiltin(..)) where

import           Control.Applicative ((<|>))
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T

import           Builtins.Builtin
import           Builtins.Guards
import           Types
import           Utils.Doc

data ListBuiltin = ListBuiltin deriving (Show)

[genDoc|listList
(list & items)

Creates a new list from the items given
|]
listList :: LVal -> Context LVal
listList = checked (properCC *> minParams 1 *> args >^ QExpr)

[genDoc|listHead
(head xs)

Returns first item in xs, calls list on argument. If xs is nil returns nil.
|]
listHead :: LVal -> Context LVal
listHead = checked (properCC *> params 1 *> (mqexpr <|> mstr))
    where
        mqexpr :: ArgCheck LVal
        mqexpr = (\p xs -> if null xs then nil p else QExpr p [head xs]) ^> argType 1 tyQExpr
        mstr :: ArgCheck LVal
        mstr = (\p str -> Str p $ if T.null str then T.empty else T.singleton (T.head str)) ^> argType 1 tyStr

[genDoc|listTail
(tail xs)

Returns possibly empty list of items after the head
|]
listTail :: LVal -> Context LVal
listTail = checked (properCC *> params 1 *> (mqexpr <|> mstr))
    where
        mqexpr :: ArgCheck LVal
        mqexpr = (\p xs -> if null xs then nil p else QExpr p $ tail xs) ^> argType 1 tyQExpr
        mstr :: ArgCheck LVal
        mstr = (\p str -> Str p $ if T.null str then T.empty else T.tail str) ^> argType 1 tyStr

[genDoc|listJoin
(join & xs)

Returns the concatenation of all supplied lists
|]
listJoin :: LVal -> Context LVal
listJoin = checked (properCC *> minParams 1 *> (mstr <|> mqexpr))
    where
        mqexpr :: ArgCheck LVal
        mqexpr = (\p xs -> QExpr p $ foldl (<>) [] xs) ^> allArgs tyQExpr
        mstr :: ArgCheck LVal
        mstr   = (\p str -> Str p $ foldl (<>) T.empty str) ^> allArgs tyStr

instance Builtin ListBuiltin where
    builtins _ = [ ("list/list", listListDoc, listList)
                 , ("list/join", listJoinDoc, listJoin)
                 , ("list/head", listHeadDoc, listHead)
                 , ("list/tail", listTailDoc, listTail)
                 ]
    globals _ = []
    initial _ = return ()
