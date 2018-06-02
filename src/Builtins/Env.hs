module Builtins.Env where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text              as T
import           System.Environment

import           Builtins.Builtin
import           Builtins.Guards
import           Types

data EnvBuiltin = EnvBuiltin deriving (Show)

lookupEnvBuiltinDoc = Just "(env/lookup x)\nLooks up an environment variable, Returns nil if it doesn't exist"
lookupEnvBuiltin :: LVal -> Context LVal
lookupEnvBuiltin = checked' (properCC *> params 1 *> argType 1 tyStr >^ fn)
    where
        fn pos s = do
            env <- liftIO $ lookupEnv (T.unpack s)
            return $ maybe (nil pos) ((Str pos) . T.pack) env

setEnvBuiltinDoc = Just "(env/set k v)\nSets the environment variable k to the value v"
setEnvBuiltin :: LVal -> Context LVal
setEnvBuiltin = checked' (properCC *> params 2 *> (argType 1 tyStr <+> argType 2 tyStr) >^ fn)
    where
        fn pos (k,v) = do
            liftIO $ setEnv (T.unpack k) (T.unpack v)
            return $ nil pos

unsetEnvBuiltinDoc = Just "(env/unset k)\nUnset the environment variable k"
unsetEnvBuiltin :: LVal -> Context LVal
unsetEnvBuiltin = checked' (properCC *> params 1 *> argType 1 tyStr >^ fn)
    where
        fn pos k = do
            liftIO $ unsetEnv (T.unpack k)
            return $ nil pos

instance Builtin EnvBuiltin where
    globals _ = []
    builtins _ = [ ("env/lookup", lookupEnvBuiltinDoc, lookupEnvBuiltin)
                 , ("env/set", setEnvBuiltinDoc, setEnvBuiltin)
                 , ("env/unset", unsetEnvBuiltinDoc, unsetEnvBuiltin)
                 ]
    initial _ = return ()
