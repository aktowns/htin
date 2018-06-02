module Builtins.List where

import           Control.Applicative  ((<|>))
import           Control.Monad.Except (throwError)
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Builtins.Builtin
import           Builtins.Guards
import           Types

data ListBuiltin = ListBuiltin deriving (Show)

builtinListDoc = Just "(list & items)\nCreates a new list from the items given"
builtinList :: LVal -> Context LVal
builtinList = checked (properCC *> minParams 1 *> args >^ QExpr)

builtinHeadDoc = Just "(head xs)\nReturns first item in xs, calls list on argument. If xs is nil returns nil."
builtinHead :: LVal -> Context LVal
builtinHead = checked (properCC *> params 1 *> (mqexpr <|> mstr))
    where
        mqexpr :: ArgCheck LVal
        mqexpr = (\p xs -> QExpr p [head xs]) ^> argType 1 tyQExpr
        mstr :: ArgCheck LVal
        mstr = (\p str -> Str p $ T.singleton (T.head str)) ^> argType 1 tyStr

builtinTailDoc = Just "(tail xs)\nReturns possibly empty list of items after the head"
builtinTail :: LVal -> Context LVal
builtinTail = checked (properCC *> params 1 *> (mqexpr <|> mstr))
    where
        mqexpr :: ArgCheck LVal
        mqexpr = (\p xs -> QExpr p $ tail xs) ^> argType 1 tyQExpr
        mstr :: ArgCheck LVal
        mstr = (\p str -> Str p $ T.tail str) ^> argType 1 tyStr

builtinJoinDoc = Just "(join & xs)\nReturns the concatenation of all supplied lists"
builtinJoin :: LVal -> Context LVal
builtinJoin = checked (properCC *> minParams 1 *> (mstr <|> mqexpr))
    where
        mqexpr :: ArgCheck LVal
        mqexpr = (\p xs -> QExpr p $ foldl (<>) [] xs) ^> allArgs tyQExpr
        mstr :: ArgCheck LVal
        mstr   = (\p str -> Str p $ foldl (<>) T.empty str) ^> allArgs tyStr

instance Builtin ListBuiltin where
    builtins _ = [ ("list", builtinListDoc, builtinList)
                 , ("join", builtinJoinDoc, builtinJoin)
                 , ("head", builtinHeadDoc, builtinHead)
                 , ("tail", builtinTailDoc, builtinTail)
                 ]
    globals _ = []
    initial _ = return ()
