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

import           Data.Maybe            (fromJust, listToMaybe, maybe)
import qualified Data.Text             as T
import           System.Directory      (doesFileExist)
import           System.Environment    (getArgs)
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

main :: IO ()
main = do
    rt <- getRunType
    prelude <- readFile "prelude.sox"
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
          evaluateSource "prelude.sox" prelude
    case res of
        Left (RuntimeException errar) -> do
            putStrLn $ "(FATAL) Runtime Exception in prelude load: "
            printPrettyError errar
        Right (_, env) | Pipe <- rt -> do
            contents <- getContents
            processFile env "stdin" contents
        Right (_, env) | File(path) <- rt -> do
            contents <- readFile path
            processFile env path contents
        Right (_, env) -> repl env
