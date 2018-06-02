module Environment where

import           Control.Monad.State
import qualified Data.Map              as M
import qualified Data.Map.Merge.Strict as MM
import           Data.Text             (Text)
import           Debug.Trace           (traceM)

import           Types

envLookup :: Text -> Context (Maybe LVal)
envLookup x = do
    (symtab, partab) <- get
    case (M.lookup x symtab, M.lookup x partab) of
        (Just v, _) -> return $ Just v -- prefer symtab
        (_, Just v) -> return $ Just v
        _           -> return $ Nothing

addSymbol :: Text -> LVal -> Context ()
addSymbol x value = do
    (symtab, partab) <- get
    put (M.insert x value symtab, partab)
    return ()

addSymbolParent :: Text -> LVal -> Context ()
addSymbolParent x value = do
    (symtab, partab) <- get
    put (symtab, M.insert x value partab)
    return ()

clonedContext :: Context LVal -> Context LVal
clonedContext c = do
    (symtab, partab) <- get

    (value, (_, par)) <- lift $ runStateT c (symtab, partab)
    let newPartab = MM.merge MM.preserveMissing MM.preserveMissing (MM.zipWithMatched $ \_ _ y -> y) partab par

    put (symtab, newPartab)
    return value

traceEnv :: Context ()
traceEnv = do
    envs <- get
    traceM $ show envs

emptyEnv :: SymTab
emptyEnv = (M.fromList [], M.fromList [])

emptyPartialEnv :: [(Text, LVal)]
emptyPartialEnv = []
