{-# LANGUAGE QuasiQuotes #-}
module Builtins.Env(EnvBuiltin(..)) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           System.Environment

import           Builtins.Builtin
import           Builtins.Guards
import           Types
import           Utils.Doc

data EnvBuiltin = EnvBuiltin deriving (Show)

[genDoc|envLookup
(env/lookup x)

Looks up an environment variable, Returns nil if it doesn't exist
|]
envLookup :: LVal -> Context LVal
envLookup = checked' $ (properCC *> params 1 *> argType 1 tyStr) >^
    \p s -> do
        env <- liftIO $ lookupEnv (T.unpack s)
        return $ maybe (nil p) ((Str p) . T.pack) env

[genDoc|envSet
(env/set k v)

Sets the environment variable k to the value v
|]
envSet :: LVal -> Context LVal
envSet = checked' $ (properCC *> params 2 *> (argType 1 tyStr <+> argType 2 tyStr)) >^
    \p (k,v) -> do
        liftIO $ setEnv (T.unpack k) (T.unpack v)
        return $ nil p

[genDoc|envUnset
(env/unset k)

Unset the environment variable k
|]
envUnset :: LVal -> Context LVal
envUnset = checked' $ (properCC *> params 1 *> argType 1 tyStr) >^
    \p k -> do
        liftIO $ unsetEnv (T.unpack k)
        return $ nil p

instance Builtin EnvBuiltin where
    globals _ = []
    builtins _ = [ ("env/lookup", envLookupDoc, envLookup)
                 , ("env/set",    envSetDoc,    envSet)
                 , ("env/unset",  envUnsetDoc,  envUnset)
                 ]
    initial _ = return ()
