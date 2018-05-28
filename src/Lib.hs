module Lib where

import           Control.Monad.State   (lift, runStateT)
import qualified Data.Text             as T
import           Debug.Trace           (traceM)
import           Text.Megaparsec       (parse)
import           Text.Megaparsec.Error (parseErrorPretty')

import           Builtins.Builtin      (addBuiltins)
import           Builtins.Core         (CoreBuiltin (..))
import           Builtins.DateTime     (DateTimeBuiltin (..))
import           Builtins.Env          (EnvBuiltin (..))
import           Builtins.Equality     (EqualityBuiltin (..))
import           Builtins.File         (FileBuiltin (..))
import           Builtins.List         (ListBuiltin (..))
import           Builtins.Math         (MathBuiltin (..))
import           Builtins.System       (SystemBuiltin (..))
import           Colour.TwentyFourBit
import           Core
import           Environment
import qualified Parser
import           Repl
import           Types

runIt :: SymTab -> Context a -> IO (a, SymTab)
runIt initialEnv fn = runStateT fn initialEnv

evaluateSource :: String -> String -> Context [LVal]
evaluateSource filename src =
    case parse Parser.exprs filename src of
        Left err -> do
            lift $ putStrLn $ parseErrorPretty' src err
            return [Err $ "Failed to parse input " <> T.pack filename]
        Right asts -> do
            lift $ ppp asts
            res <- traverse eval asts
            return $ filter (\x -> x /= SExpr [] && x /= QExpr []) res

someFunc :: IO ()
someFunc = do
    prelude <- readFile "prelude.tin"
    (r, env) <- runIt emptyEnv $ do
          addBuiltins CoreBuiltin
          addBuiltins ListBuiltin
          addBuiltins MathBuiltin
          addBuiltins FileBuiltin
          addBuiltins DateTimeBuiltin
          addBuiltins EqualityBuiltin
          addBuiltins EnvBuiltin
          addBuiltins SystemBuiltin
          evaluateSource "prelude.tin" prelude
    if null r then putStrLn "" else print r
    repl env
