module Builtins.Builtin where

import           Debug.Trace (traceM)
import           Environment
import           Types

class (Show a) => Builtin a where
    builtins :: a -> [(String, Maybe String, LBuiltin)]
    globals  :: a -> [(String, Maybe String, Context LVal)]
    initial  :: a -> Context ()

addBuiltin :: String -> Maybe String -> LBuiltin -> Context ()
addBuiltin x doc fn = addSymbolParent x $ Builtin doc fn

addBuiltin' :: String -> Maybe String -> Context LVal -> Context ()
addBuiltin' x doc fn = addSymbolParent x $ BuiltinVar doc fn

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
