module Builtins.Env where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text              as T
import           System.Environment

import           Builtins.Builtin
import           Types

data EnvBuiltin = EnvBuiltin deriving (Show)

lookupEnvBuiltinDoc = Just "(env/lookup x)\nLooks up an environment variable, Returns nil if it doesn't exist"
lookupEnvBuiltin :: LVal -> Context LVal
lookupEnvBuiltin (SExpr _ xs)
    | (Str p s) <- head xs = do
        env <- liftIO $ lookupEnv (T.unpack s)
        return $ maybe (QExpr p []) ((Str p) . T.pack) env

setEnvBuiltinDoc = Just "(env/set k v)\nSets the environment variable k to the value v"
setEnvBuiltin :: LVal -> Context LVal
setEnvBuiltin (SExpr _ xs)
    | (Str p k') <- head xs, (Str _ v') <- xs !! 1 = do
        liftIO $ setEnv (T.unpack k') (T.unpack v')
        return $ QExpr p []

unsetEnvBuiltinDoc = Just "(env/unset k)\nUnset the environment variable k"
unsetEnvBuiltin :: LVal -> Context LVal
unsetEnvBuiltin (SExpr _ xs)
    | (Str p k') <- head xs = do
        liftIO $ unsetEnv (T.unpack k')
        return $ QExpr p []

instance Builtin EnvBuiltin where
    globals _ = []
    builtins _ = [ ("env/lookup", lookupEnvBuiltinDoc, lookupEnvBuiltin)
                 , ("env/set", setEnvBuiltinDoc, setEnvBuiltin)
                 , ("env/unset", unsetEnvBuiltinDoc, unsetEnvBuiltin)
                 ]
    initial _ = return ()
