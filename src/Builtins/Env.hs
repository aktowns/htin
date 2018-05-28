module Builtins.Env where

import           Control.Monad.State (lift)
import qualified Data.Text           as T
import           System.Environment

import           Builtins.Builtin
import           Types

data EnvBuiltin = EnvBuiltin deriving (Show)

lookupEnvBuiltinDoc = Just "(env/lookup x)\nLooks up an environment variable, Returns nil if it doesn't exist"
lookupEnvBuiltin :: LVal -> Context LVal
lookupEnvBuiltin (SExpr xs)
    | (Str s) <- head xs = do
        env <- lift $ lookupEnv (T.unpack s)
        return $ maybe (QExpr []) (Str . T.pack) env

setEnvBuiltinDoc = Just "(env/set k v)\nSets the environment variable k to the value v"
setEnvBuiltin :: LVal -> Context LVal
setEnvBuiltin (SExpr xs)
    | (Str k') <- head xs, (Str v') <- xs !! 1 = do
        lift $ setEnv (T.unpack k') (T.unpack v')
        return $ QExpr []

unsetEnvBuiltinDoc = Just "(env/unset k)\nUnset the environment variable k"
unsetEnvBuiltin :: LVal -> Context LVal
unsetEnvBuiltin (SExpr xs)
    | (Str k') <- head xs = do
        lift $ unsetEnv (T.unpack k')
        return $ QExpr []

instance Builtin EnvBuiltin where
    globals _ = []
    builtins _ = [ ("env/lookup", lookupEnvBuiltinDoc, lookupEnvBuiltin)
                 , ("env/set", setEnvBuiltinDoc, setEnvBuiltin)
                 , ("env/unset", unsetEnvBuiltinDoc, unsetEnvBuiltin)
                 ]
    initial _ = return ()
