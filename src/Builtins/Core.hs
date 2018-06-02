{-# LANGUAGE RecordWildCards #-}
module Builtins.Core where

import           Control.Applicative    ((<|>))
import           Control.Monad          (when)
import           Control.Monad.Except   (catchError, throwError)
import           Control.Monad.IO.Class (liftIO)
import           Data.Bifoldable        (bifold)
import           Data.Foldable          (traverse_)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Debug.Trace            (traceM)
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.Megaparsec        (parse)
import           Text.Megaparsec.Error  (parseErrorPretty')

import           Builtins.Builtin
import           Builtins.Guards
import           Core                   (call, eval)
import           Environment            (addSymbol, addSymbolParent, emptyPartialEnv)
import qualified Parser
import           PrettyPrint
import           Types

data CoreBuiltin = CoreBuiltin deriving (Show)

data Scope = Global | Local

isGlobal :: Scope -> Bool
isGlobal Global = True
isGlobal _      = False

searchForFile :: FilePath -> [FilePath] -> IO (Maybe FilePath)
searchForFile path []     = return Nothing
searchForFile path (x:xs) = do
    let fullPath = x </> path
    exists <- doesFileExist fullPath
    if exists then return (Just fullPath)
    else searchForFile path xs

searchPaths :: IO [FilePath]
searchPaths = do
    paths <- lookupEnv "TIN_PATH"
    current <- getCurrentDirectory
    case paths of
        Nothing -> return [current]
        Just xs -> return $ current : splitSearchPath xs

builtinLambdaDoc = Just "(\\ doc? args body)\nReturns a lambda with an optional string documentation line\n\
                        \if the args contains & and is proceeded by one final argument, all remaining arguments\n\
                        \are returned as a list with the name of that symbol."
builtinLambda :: LVal -> Context LVal
builtinLambda = checked (properCC *> (withDoc <|> withoutDoc))
    where
        withDoc = params 3 *> argType 1 tyStr <+> argTypeCheckInner 2 tySym <+> argTypeCheck 3 tyQExpr >^ \pos ((doc, formals), body) ->
            Lambda pos emptyPartialEnv (Just doc) formals body
        withoutDoc = params 2 *> argTypeCheckInner 1 tySym <+> argTypeCheck 2 tyQExpr >^ \pos (formals, body) ->
            Lambda pos emptyPartialEnv Nothing formals body

builtinVar :: Scope -> LVal -> Context LVal
builtinVar scope = checked' $ (properCC *> minParams 2 *> argTypeInner 1 tySym <+> restArgs 2) >^ \pos (syms, rest) -> do
    let assoc = zip syms rest
    if isGlobal scope then traverse_ (\(k, v) -> addSymbolParent k v) assoc
    else traverse_ (\(k, v) -> addSymbol k v) assoc
    return $ nil pos

builtinDefDoc = Just "(def names values)\nGiven a list of names and a list of values def defines them in the global scope"
builtinDef :: LVal -> Context LVal
builtinDef = builtinVar Global

builtinAssDoc = Just "(= names values)\nGiven a list of names and a list of values = defines them in the local scope"
builtinAss :: LVal -> Context LVal
builtinAss = builtinVar Local

builtinEvalDoc = Just "(eval list)\nEvaluates the given list returning the result"
builtinEval :: LVal -> Context LVal
builtinEval = checked' $ (properCC *> params 1 *> argType 1 tyQExpr) >^ \pos xs -> eval $ SExpr pos xs

builtinCatchDoc = Just "(catch body handler)\nEvaluate body, if an error is raised catch it in handler"
builtinCatch :: LVal -> Context LVal
builtinCatch = checked' $ (properCC *> params 2 *> argType 1 tyQExpr <+> argTypeCheck 2 tyLambda) >^ \pos (xs', f) ->
    catchError (eval $ SExpr pos xs') $ \(RuntimeException (Err _ errar)) -> call f (SExpr pos [Str pos $ errar])

builtinLoadDoc = Just "(load path)\nLoads the given file into the current environment"
builtinLoad :: Bool -> LVal -> Context LVal
builtinLoad verbose = checked' $ (properCC *> params 1 *> argType 1 tyStr) >^ \pos path -> do
    searchDirs <- liftIO searchPaths
    maybeFile <- liftIO $ searchForFile (T.unpack path) searchDirs
    case maybeFile of
      Nothing -> return $ Boolean pos False
      Just file -> do
        liftIO $ when verbose $ putStrLn $ "loading file " ++ file
        contents <- liftIO $ readFile file
        case parse Parser.exprs file contents of
            Left err -> do
                liftIO $ putStrLn (parseErrorPretty' contents err)
                return $ Boolean pos False
            Right x  -> do
                ok <- flip traverse x $ \expr -> do
                    evald <- eval expr
                    if isErr evald then throwError $ RuntimeException evald
                    else return True
                return $ if all (== True) ok then Boolean pos True else Boolean pos False


builtinIfDoc = Just "(if condition truebody falsebody)\nEvaluates the given condition and if #t evalutes truebody otherwise evaluates falsebody"
builtinIf :: LVal -> Context LVal
builtinIf = checked' $ (properCC *> params 3 *> (checkBool <|> checkQExpr <|> checkNum) <+> argType 2 tyQExpr <+> argType 3 tyQExpr) >^ \pos ((cond, tb), fb) ->
        eval $ SExpr pos $ if cond then tb else fb
    where
        checkBool  = argType 1 tyBool >^ (\_ b -> b)
        checkQExpr = argType 1 tyQExpr >^ (\_ q -> length q == 0)
        checkNum   = argType 1 tyNum >^ (\_ q -> q >= 1)

builtinPrintlnDoc = Just "(println x & xs)\nPrints the given arguments to standard out, ending with a newline"
builtinPrintln :: LVal -> Context LVal
builtinPrintln = checked' $ (properCC *> minParams 1 *> args) >^ \pos xs -> do
    printed <- traverse printer xs
    liftIO $ T.putStrLn $ T.unwords printed
    return $ nil pos

builtinPrintDoc = Just "(print x & xs)\nPrints the given arguments to standard out"
builtinPrint :: LVal -> Context LVal
builtinPrint = checked' $ (properCC *> minParams 1 *> args) >^ \pos xs -> do
    printed <- traverse printer xs
    liftIO $ T.putStr $ T.unwords printed
    return $ nil pos

builtinShowDoc = Just "(show x & xs)\nShow string representation for hte supplied types"
builtinShow :: LVal -> Context LVal
builtinShow = checked' $ (properCC *> minParams 1 *> args) >^ \pos xs ->
    Str pos . T.unwords <$> traverse printer xs

printer :: LVal -> Context Text
printer (Str _ s)     = return s
printer (Num _ i)     = return $ tshow i
printer (Boolean _ b) = return $ if b then "#t" else "#f"
printer s@(Sym _ d)   = eval s >>= printer
printer (QExpr _ xs)  = (\x -> "{" <> x <> "}") <$> T.unwords <$> traverse printer xs
printer (Err _ e)     = return $ e

builtinErrorDoc = Just "(error str)\nReturns an error with the value str"
builtinError :: LVal -> Context LVal
builtinError = checked $ (properCC *> params 1 *> argType 1 tyStr) >^ Err

instance Builtin CoreBuiltin where
    builtins _ = [ ("eval", builtinEvalDoc, builtinEval)
                 , ("catch", builtinCatchDoc, builtinCatch)
                 , ("load", builtinLoadDoc, builtinLoad False)
                 , ("load-verbose", builtinLoadDoc, builtinLoad True)
                 , ("if", builtinIfDoc, builtinIf)
                 , ("\\", builtinLambdaDoc, builtinLambda)
                 , ("def", builtinDefDoc, builtinDef)
                 , ("=", builtinAssDoc, builtinAss)
                 , ("println", builtinPrintlnDoc, builtinPrintln)
                 , ("print", builtinPrintDoc, builtinPrint)
                 , ("show", builtinShowDoc, builtinShow)
                 , ("error", builtinErrorDoc, builtinError)
                 ]
    globals _ = [ ("version", Just "shows the version", return $ QExpr builtinPos [Num builtinPos 0, Num builtinPos 0, Num builtinPos 1])
                , ("help", Just "show the help", return $ Str builtinPos "The help is a lie")]
    initial _ = return ()
