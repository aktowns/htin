module Lib where

import           Control.Monad.Except   (runExceptT)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (runStateT)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Text.Megaparsec        (parse)
import           Text.Megaparsec.Error  (parseErrorPretty')

import           Builtins.Builtin       (addBuiltins)
import           Builtins.Core          (CoreBuiltin (..))
import           Builtins.DateTime      (DateTimeBuiltin (..))
import           Builtins.Env           (EnvBuiltin (..))
import           Builtins.Equality      (EqualityBuiltin (..))
import           Builtins.FFI           (FFIBuiltin (..))
import           Builtins.File          (FileBuiltin (..))
import           Builtins.List          (ListBuiltin (..))
import           Builtins.Math          (MathBuiltin (..))
import           Builtins.System        (SystemBuiltin (..))
import           Colour.TwentyFourBit
import           Core
import           Environment
import qualified Parser
import           PrettyPrint
import           Repl
import           Types

runIt :: SymTab -> Context a -> IO (Either RuntimeException (a, SymTab))
runIt initialEnv fn = runExceptT $ runStateT fn initialEnv

evaluateSource :: String -> String -> Context [LVal]
evaluateSource filename src =
    case parse Parser.exprs filename src of
        Left error -> do
            liftIO $ putStrLn $ parseErrorPretty' src error
            return [Boolean builtinPos False]
        Right asts -> do
            liftIO $ ppp asts
            res <- traverse eval asts
            let filtered = filter isErr res
            return filtered

someFunc :: IO ()
someFunc = do
    prelude <- readFile "prelude.tin"
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
          evaluateSource "prelude.tin" prelude
    case res of
        Left (RuntimeException errar) -> do
            putStrLn $ "(FATAL) Runtime Exception in prelude load: "
            printPrettyError errar
        Right (r, env) -> repl env
