module Builtins.Builtin where

import           Data.Text   (Text)
import           Debug.Trace (traceM)

import           Environment
import           Types

class (Show a) => Builtin a where
    builtins :: a -> [(Text, Maybe Text, LBuiltin)]
    globals  :: a -> [(Text, Maybe Text, Context LVal)]
    initial  :: a -> Context ()

addBuiltin :: Text -> Maybe Text -> LBuiltin -> Context ()
addBuiltin x doc fn = addSymbolParent x $ Builtin builtinPos doc fn

addBuiltin' :: Text -> Maybe Text -> Context LVal -> Context ()
addBuiltin' x doc fn = addSymbolParent x $ BuiltinVar builtinPos doc fn

addBuiltins :: Builtin a => a -> Context ()
addBuiltins builtin = do
    traceM $ "loading globals for " ++ show builtin
    addGlobals' $ globals builtin
    traceM $ "loading builtins for " ++ show builtin
    addBuiltins' $ builtins builtin
    traceM $ "loading initializer for " ++ show builtin
    initial builtin
    where
        addBuiltins' []           = return ()
        addBuiltins' ((k,d,v):xs) = addBuiltin k d v >> addBuiltins' xs
        addGlobals' []           = return ()
        addGlobals' ((k,d,v):xs) = addBuiltin' k d v >> addGlobals' xs
