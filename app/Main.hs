module Main where

import           Builtins.Builtin      (addBuiltins)
import           Builtins.Core         (CoreBuiltin (..))
import           Builtins.DateTime     (DateTimeBuiltin (..))
import           Builtins.Env          (EnvBuiltin (..))
import           Builtins.Equality     (EqualityBuiltin (..))
import           Builtins.FFI          (FFIBuiltin (..))
import           Builtins.File         (FileBuiltin (..))
import           Builtins.List         (ListBuiltin (..))
import           Builtins.Math         (MathBuiltin (..))
import           Builtins.System       (SystemBuiltin (..))
import           Environment
import           Lib                   (evaluateSource, runIt)
import           Repl
import           Types

import           Data.List             (isPrefixOf)
import           Data.Maybe            (fromJust, fromMaybe, listToMaybe, maybe)
import qualified Data.Text             as T
import           System.Directory      (doesFileExist, getCurrentDirectory)
import           System.Environment    (getArgs, lookupEnv)
import           System.FilePath.Posix ((</>))
import           System.Posix.Terminal (queryTerminal)
import           System.Posix.Types    (Fd (..))

data RunType = Repl | File String | Pipe deriving (Show, Eq)

getRunType :: IO RunType
getRunType = do
    isTerm <- queryTerminal $ Fd 0
    args <- listToMaybe <$> getArgs
    fileExists <- maybe (return False) doesFileExist args

    if fileExists then return $ File (fromJust args)
    else if not isTerm then return Pipe
    else return Repl

processFile :: SymTab -> String -> String -> IO ()
processFile env name contents = do
    res' <- runIt env $ evaluateSource name contents
    case res' of
        Left (RuntimeException exc) -> do
            errar <- printPrettyError' exc
            putStr $ T.unpack errar
        Right (_, _) -> return ()

findPrelude :: IO (Maybe FilePath)
findPrelude = do
    maybeFile <- lookupEnv "SOCKS_PRELUDE"
    exists <- maybe (return False) doesFileExist maybeFile
    if exists then return $ maybeFile
    else do
        path <- getCurrentDirectory
        let curpath = path </> "prelude.sox"
        localExists <- doesFileExist curpath
        if localExists then return $ Just curpath
        else return Nothing

main :: IO ()
main = do
    rt <- getRunType
    preludeFile' <- findPrelude
    let preludeFile = fromMaybe (error "Unable to find a prelude.sox, have you set SOCKS_PRELUDE?") preludeFile'
    prelude <- readFile preludeFile
    res <- runIt emptyEnv $ do
          addBuiltins CoreBuiltin
          addBuiltins ListBuiltin
          addBuiltins MathBuiltin
          addBuiltins FileBuiltin
          addBuiltins DateTimeBuiltin
          addBuiltins EqualityBuiltin
          addBuiltins EnvBuiltin
          addBuiltins SystemBuiltin
          addBuiltins FFIBuiltin
          evaluateSource preludeFile prelude
    case res of
        Left (RuntimeException errar) -> do
            putStrLn $ "(FATAL) Runtime Exception in prelude load: "
            printPrettyError errar
        Right (_, env) | Pipe <- rt -> do
            contents <- getContents
            processFile env "stdin" contents
        Right (_, env) | File(path) <- rt -> do
            actualPath <- if isPrefixOf "/" path then return path
                          else do
                            cwd <- getCurrentDirectory
                            return $ cwd </> path
            contents <- readFile actualPath
            processFile env actualPath contents
        Right (_, env) -> repl env
