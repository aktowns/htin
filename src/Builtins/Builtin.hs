module Builtins.Builtin where

import           Data.Text   (Text)

import           Environment
import           Types

class (Show a) => Builtin a where
    builtins :: a -> [(Text, Maybe Text, LBuiltin)]
    globals  :: a -> [(Text, Maybe Text, Context LVal)]
    initial  :: a -> Context ()

addBuiltin :: Text -> Maybe Text -> LBuiltin -> Context ()
addBuiltin x bdoc fun = addSymbolParent x $ Builtin builtinPos bdoc fun

addBuiltin' :: Text -> Maybe Text -> Context LVal -> Context ()
addBuiltin' x bdoc fun = addSymbolParent x $ BuiltinVar builtinPos bdoc fun

addBuiltins :: Builtin a => a -> Context ()
addBuiltins builtin = do
    addGlobals' $ globals builtin
    addBuiltins' $ builtins builtin
    initial builtin
    where
        addBuiltins' []           = return ()
        addBuiltins' ((k,d,v):xs) = addBuiltin k d v >> addBuiltins' xs
        addGlobals' []           = return ()
        addGlobals' ((k,d,v):xs) = addBuiltin' k d v >> addGlobals' xs
