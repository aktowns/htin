module Repl where

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.State      (lift, runStateT)
import           Data.IORef
import           Data.List                (isPrefixOf)
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

repl :: SymTab -> IO ()
repl env = do
    completions <- newIORef env
    runInputT (settings completions) $ withInterrupt (loop completions env)
    where
        loop :: Completions -> SymTab -> InputT IO ()
        loop ref env' = do
            lift $ modifyIORef ref (const env')
            minput <- handle (\Interrupt -> return $ Just "interrupted") $ getInputLine "% "
            case minput of
                Nothing     -> return ()
                Just "quit" -> return ()
                Just "interrupted" -> loop ref env'
                Just input  -> do
                    newenv <- case parse Parser.expr "stdin" input of
                        Left err  -> do
                            outputStrLn $ parseErrorPretty' input err
                            return env'
                        Right ast -> do
                            lift $ cursorUpLine 1
                            lift $ hFlush stdout
                            outputStrLn $ "\r% " ++ T.unpack (pp ast)
                            (val,ne) <- lift $ runStateT (eval ast) env'
                            outputStrLn $ T.unpack (pp val)
                            return ne
                    loop ref newenv
