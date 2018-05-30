{-# LANGUAGE RecordWildCards #-}
module Builtins.Core where

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
builtinLambda (SExpr c xs)
    | hasDoc, (Str _ doc) <- head xs = return $ Lambda c emptyPartialEnv (Just doc) (xs !! 1) (xs !! 2)
    | not $ isQExpr (head xs)        = return $ err c "argument list should be a QExpr"
    | not $ isQExpr (xs !! 1)        = return $ err c "body list should be a QExpr"
    | not $ checkFormals (head xs)   = return $ err c "argument list should be all symbols"
    | otherwise                      = return $ Lambda c emptyPartialEnv Nothing (head xs) (xs !! 1)
    where
        checkFormals (QExpr _ xs') = all isSym xs'
        hasDoc = length xs == 3 && isStr (head xs) && isQExpr (xs !! 1) && isQExpr (xs !! 2)

builtinVar :: Scope -> LVal -> Context LVal
builtinVar scope (SExpr c xs) = do
    let (QExpr _ syms) = head xs
    let rest = tail xs
    let assi = zip syms rest
    if isGlobal scope then traverse_ (\ (Sym _ k, v) -> addSymbolParent k v) assi
    else traverse_ (\ (Sym _ k, v) -> addSymbol k v) assi
    return $ SExpr c []

builtinDefDoc = Just "(def names values)\nGiven a list of names and a list of values def defines them in the global scope"
builtinDef :: LVal -> Context LVal
builtinDef = builtinVar Global

builtinAssDoc = Just "(= names values)\nGiven a list of names and a list of values = defines them in the local scope"
builtinAss :: LVal -> Context LVal
builtinAss = builtinVar Local

builtinEvalDoc = Just "(eval list)\nEvaluates the given list returning the result"
builtinEval :: LVal -> Context LVal
builtinEval (SExpr c xs)
    | (QExpr p xs') <- head xs = eval $ SExpr p xs'
    | e@(Err _ _) <- head xs   = throwError (RuntimeException e)
    | otherwise = return $ err c $ "eval handed incorrect arguments, was given: " <> tshow xs

builtinCatchDoc = Just "(catch body handler)\nEvaluate body, if an error is raised catch it in handler"
builtinCatch :: LVal -> Context LVal
builtinCatch (SExpr c xs)
    | (QExpr p xs') <- head xs, f@Lambda{..} <- xs !! 1 = do
        catchError (eval $ SExpr p xs') $ \(RuntimeException errar) -> call f (SExpr p [errar])
    | otherwise = return $ err c $ "catch handed incorrect arguments, was given: " <> tshow xs

builtinLoadDoc = Just "(load path)\nLoads the given file into the current environment"
builtinLoad :: Bool -> LVal -> Context LVal
builtinLoad verbose (SExpr _ xs)
    | (Str p path) <- head xs = do
        searchDirs <- liftIO searchPaths
        maybeFile <- liftIO $ searchForFile (T.unpack path) searchDirs
        case maybeFile of
          Nothing -> return $ Boolean p False
          Just file -> do
            liftIO $ when verbose $ putStrLn $ "loading file " ++ file
            contents <- liftIO $ readFile file
            case parse Parser.exprs file contents of
                Left err -> do
                    liftIO $ putStrLn (parseErrorPretty' contents err)
                    return $ Boolean p False
                Right x  -> do
                    ok <- flip traverse x $ \expr -> do
                        evald <- eval expr
                        if isErr evald then throwError $ RuntimeException evald
                        else return True
                    return $ if all (== True) ok then Boolean p True else Boolean p False

builtinIfDoc = Just "(if condition truebody falsebody)\nEvaluates the given condition and if #t evalutes truebody otherwise evaluates falsebody"
builtinIf :: LVal -> Context LVal
builtinIf (SExpr c xs)
    | (Boolean _ n) <- head xs, (QExpr _ tb) <- xs !! 1, (QExpr _ fb) <- xs !! 2 = eval $ SExpr c $ if n then tb else fb
    | (QExpr _ [])  <- head xs, (QExpr _ tb) <- xs !! 1, (QExpr _ fb) <- xs !! 2 = eval $ SExpr c fb
    | (Num _ n)     <- head xs, (QExpr _ tb) <- xs !! 1, (QExpr _ fb) <- xs !! 2 = eval $ SExpr c $ if n >= 1 then tb else fb
    | otherwise = return $ err c $ "if handed incorrect arguments, was given: " <> tshow xs

builtinPrintlnDoc = Just "(println x & xs)\nPrints the given arguments to standard out, ending with a newline"
builtinPrintln :: LVal -> Context LVal
builtinPrintln (SExpr c xs) = do
    printed <- traverse printer xs
    liftIO $ T.putStrLn $ T.unwords printed
    return $ QExpr c []

builtinPrintDoc = Just "(print x & xs)\nPrints the given arguments to standard out"
builtinPrint :: LVal -> Context LVal
builtinPrint (SExpr c xs) = do
    printed <- traverse printer xs
    liftIO $ T.putStr $ T.unwords printed
    return $ QExpr c []

builtinShowDoc = Just "(show x & xs)\nShow string representation for hte supplied types"
builtinShow :: LVal -> Context LVal
builtinShow (SExpr c xs) = do
    printed <- traverse printer xs
    return $ Str c $ T.unwords printed

printer :: LVal -> Context Text
printer (Str _ s)     = return s
printer (Num _ i)     = return $ tshow i
printer (Boolean _ b) = return $ if b then "#t" else "#f"
printer s@(Sym _ d)   = eval s >>= printer
printer (QExpr _ xs)  = return $ tshow xs
printer (Err _ e)     = return $ e

builtinErrorDoc = Just "(error str)\nReturns an error with the value str"
builtinError :: LVal -> Context LVal
builtinError (SExpr c xs)
    | (Str p error) <- head xs = return $ err p error
    | otherwise = return $ err c $ "error handed incorrect arguments, recieved " <> tshow xs

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
