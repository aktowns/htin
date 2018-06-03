module Repl where

import           Control.Monad.Except     (runExceptT)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.State      (runStateT)
import           Data.Char                (isSpace)
import           Data.IORef
import           Data.List                (dropWhile, dropWhileEnd, isPrefixOf)
import qualified Data.Map                 as M
import qualified Data.Text                as T
import           System.Console.ANSI
import           System.Console.Haskeline
import           System.IO                (hFlush, stdout)
import           Text.Megaparsec          (parse)
import           Text.Megaparsec.Error    (parseErrorPretty')

import           Core
import qualified Parser
import           PrettyPrint
import           Types

type Completions = IORef SymTab

settings :: (MonadIO m) => Completions -> Settings m
settings ref = Settings { autoAddHistory = True
                        , historyFile    = Just ".tin_history"
                        , complete       = completeWord Nothing [' ','\t','\n','(',')'] (completion ref)
                        }

completion :: (MonadIO m) => Completions -> String -> m [Completion]
completion ref str = do
    compls <- liftIO $ readIORef ref
    let prefixed = filter (str `isPrefixOf`) (map T.unpack $ M.keys $ snd compls)
    return $ map simpleCompletion prefixed

trim :: [Char] -> [Char]
trim = dropWhileEnd isSpace . dropWhile isSpace

repl :: SymTab -> IO ()
repl env = do
    completions <- newIORef env
    runInputT (settings completions) $ withInterrupt (loop completions env)
    where
        loop :: Completions -> SymTab -> InputT IO ()
        loop ref env' = do
            liftIO $ modifyIORef ref (const env')
            minput <- handle (\Interrupt -> return $ Just "interrupted") $ getInputLine "% "
            case trim <$> minput of
                Nothing     -> return ()
                Just "quit" -> return ()
                Just "interrupted" -> loop ref env'
                Just "" -> loop ref env'
                Just input  -> do
                    newenv <- case parse Parser.expr "stdin" input of
                        Left errar  -> do
                            outputStrLn $ parseErrorPretty' input errar
                            return env'
                        Right ast -> do
                            liftIO $ cursorUpLine 1
                            liftIO $ hFlush stdout
                            outputStrLn $ "\r% " ++ T.unpack (pp ast)
                            res <- liftIO $ runExceptT $ runStateT (eval ast) env'
                            case res of
                                Left (RuntimeException exc) -> do
                                    errar <- liftIO $ printPrettyError' exc
                                    outputStr $ T.unpack errar
                                    return env'
                                Right (value, ne) -> do
                                    outputStrLn $ T.unpack (pp value)
                                    return ne
                    loop ref newenv
