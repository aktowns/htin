module Lib where

import           Control.Monad          (when)
import           Control.Monad.Except   (runExceptT)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (runStateT)
import           Data.Maybe             (listToMaybe)
import           System.Directory       (doesFileExist)
import           System.Environment     (getArgs)
import           System.Posix.Terminal  (queryTerminal)
import           System.Posix.Types     (Fd (..))
import           Text.Megaparsec        (parse)
import           Text.Megaparsec.Error  (parseErrorPretty')

import           Core
import qualified Parser
import           PrettyPrint
import           Types

isInteractive :: IO Bool
isInteractive = do
    isTerm <- queryTerminal $ Fd 0
    appArgs <- listToMaybe <$> getArgs
    fileExists <- maybe (return False) doesFileExist appArgs

    return $ isTerm && not fileExists

runIt :: SymTab -> Context a -> IO (Either RuntimeException (a, SymTab))
runIt initialEnv fun = runExceptT $ runStateT fun initialEnv

evaluateSource :: String -> String -> Context [LVal]
evaluateSource filename src =
    case parse Parser.exprs filename src of
        Left errar -> do
            liftIO $ putStrLn $ parseErrorPretty' src errar
            return [Boolean builtinPos False]
        Right asts -> do
            interactive <- liftIO $ isInteractive
            when interactive $ liftIO (ppp asts)
            res <- traverse eval asts
            let filtered = filter isErr res
            return filtered
