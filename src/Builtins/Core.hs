{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
module Builtins.Core(CoreBuiltin(..)) where

import           Control.Applicative    ((<|>))
import           Control.Monad          (when)
import           Control.Monad.Except   (catchError, throwError)
import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable          (traverse_)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
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
import           Types
import           Utils.Doc

data CoreBuiltin = CoreBuiltin deriving (Show)

data Scope = Global | Local

isGlobal :: Scope -> Bool
isGlobal Global = True
isGlobal _      = False

searchForFile :: FilePath -> [FilePath] -> IO (Maybe FilePath)
searchForFile _ []     = return Nothing
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

[genDoc|coreLambda
(core/\ doc? args body)

Returns a lambda with an optional string documentation line
if the args contains & and is proceeded by one final argument, all remaining arguments
are returned as a list with the name of that symbol.
|]
coreLambda :: LVal -> Context LVal
coreLambda = checked (properCC *> (withDoc <|> withoutDoc))
    where
        withDoc = params 3 *> argType 1 tyStr <+> argTypeCheckInner 2 tySym <+> argTypeCheck 3 tyQExpr >^
            \p ((doc, formals), body) -> Lambda p emptyPartialEnv (Just doc) formals body
        withoutDoc = params 2 *> argTypeCheckInner 1 tySym <+> argTypeCheck 2 tyQExpr >^
            \p (formals, body) -> Lambda p emptyPartialEnv Nothing formals body

coreVar :: Scope -> LVal -> Context LVal
coreVar scope = checked' $ (properCC *> minParams 2 *> argTypeInner 1 tySym <+> restArgs 2) >^
    \p (syms, rest) -> do
        let assoc = zip syms rest
        if isGlobal scope then traverse_ (\(k, v) -> addSymbolParent k v) assoc
        else traverse_ (\(k, v) -> addSymbol k v) assoc
        return $ nil p

[genDoc|coreDef
(core/def names values)

Given a list of names and a list of values def defines them in the global scope
|]
coreDef :: LVal -> Context LVal
coreDef = coreVar Global

[genDoc|coreAss
(core/= names values)

Given a list of names and a list of values = defines them in the local scope
|]
coreAss :: LVal -> Context LVal
coreAss = coreVar Local

[genDoc|coreEval
(core/eval list)

Evaluates the given list returning the result
|]
coreEval :: LVal -> Context LVal
coreEval = checked' $ (properCC *> params 1 *> argType 1 tyQExpr) >^
    \p xs -> eval $ SExpr p xs

[genDoc|coreCatch
(core/catch body handler)

Evaluate body, if an error is raised catch it in handler
|]
coreCatch :: LVal -> Context LVal
coreCatch = checked' $ (properCC *> params 2 *> argType 1 tyQExpr <+> argTypeCheck 2 tyLambda) >^
    \p (xs', f) ->
        catchError (eval $ SExpr p xs') $ \exc -> call f (SExpr p [Str p $ unsafeError exc])

[genDoc|coreLoad
(core/load path)

Loads the given file into the current environment
|]
coreLoad :: Bool -> LVal -> Context LVal
coreLoad verbose = checked' $ (properCC *> params 1 *> argType 1 tyStr) >^ \p path -> do
    searchDirs <- liftIO searchPaths
    maybeFile <- liftIO $ searchForFile (T.unpack path) searchDirs
    case maybeFile of
      Nothing -> return $ Boolean p False
      Just file -> do
        liftIO $ when verbose $ putStrLn $ "loading file " ++ file
        contents <- liftIO $ readFile file
        case parse Parser.exprs file contents of
            Left errar -> do
                liftIO $ putStrLn (parseErrorPretty' contents errar)
                return $ Boolean p False
            Right x  -> do
                ok <- flip traverse x $ \expr -> do
                    evald <- eval expr
                    if isErr evald then throwError $ RuntimeException evald
                    else return True
                return $ if all (== True) ok then Boolean p True else Boolean p False


[genDoc|coreIf
(core/if condition truebody falsebody)

Evaluates the given condition and if #t evalutes truebody otherwise evaluates falsebody
|]
coreIf :: LVal -> Context LVal
coreIf = checked' $ (properCC *> params 3 *> (checkBool <|> checkQExpr <|> checkNum) <+> argType 2 tyQExpr <+> argType 3 tyQExpr) >^
    \p ((cond, tb), fb) -> eval $ SExpr p $ if cond then tb else fb
    where
        checkBool  = argType 1 tyBool >^ (\_ b -> b)
        checkQExpr = argType 1 tyQExpr >^ (\_ q -> length q == 0)
        checkNum   = argType 1 tyNum >^ (\_ q -> q >= 1)

[genDoc|corePrintln
(core/println x & xs)

Prints the given arguments to standard out, ending with a newline
|]
corePrintln :: LVal -> Context LVal
corePrintln = checked' $ (properCC *> minParams 1 *> args) >^
    \p xs -> do
        printed <- traverse printer xs
        liftIO $ T.putStrLn $ T.unwords printed
        return $ nil p

[genDoc|corePrint
(core/print x & xs)

Prints the given arguments to standard out
|]
corePrint :: LVal -> Context LVal
corePrint = checked' $ (properCC *> minParams 1 *> args) >^
    \p xs -> do
        printed <- traverse printer xs
        liftIO $ T.putStr $ T.unwords printed
        return $ nil p

[genDoc|coreShow
(core/show x & xs)

Show string representation for hte supplied types
|]
coreShow :: LVal -> Context LVal
coreShow = checked' $ (properCC *> minParams 1 *> args) >^
    \p xs -> Str p . T.unwords <$> traverse printer xs

printer :: LVal -> Context Text
printer (Str _ s)     = return s
printer (Num _ i)     = return $ tshow i
printer (Boolean _ b) = return $ if b then "#t" else "#f"
printer s@(Sym _ _)   = eval s >>= printer
printer (QExpr _ xs)  = (\x -> "{" <> x <> "}") <$> T.unwords <$> traverse printer xs
printer (Err _ e)     = return $ e
printer _             = error "Printer given unprintable value"

[genDoc|coreError
(core/error str)

Returns an error with the value str
|]
coreError :: LVal -> Context LVal
coreError = checked $ (properCC *> params 1 *> argType 1 tyStr) >^ Err

instance Builtin CoreBuiltin where
    builtins _ = [ ("core/catch",        coreCatchDoc,   coreCatch)
                 , ("core/def",          coreDefDoc,     coreDef)
                 , ("core/error",        coreErrorDoc,   coreError)
                 , ("core/eval",         coreEvalDoc,    coreEval)
                 , ("core/if",           coreIfDoc,      coreIf)
                 , ("core/load",         coreLoadDoc,    coreLoad False)
                 , ("core/load-verbose", coreLoadDoc,    coreLoad True)
                 , ("core/print",        corePrintDoc,   corePrint)
                 , ("core/println",      corePrintlnDoc, corePrintln)
                 , ("core/show",         coreShowDoc,    coreShow)
                 , ("core/\\",           coreLambdaDoc,  coreLambda)
                 , ("core/=",            coreAssDoc,     coreAss)
                 ]
    globals _ = [ ("version", Just "shows the version", return $ QExpr builtinPos [Num builtinPos 0, Num builtinPos 0, Num builtinPos 1])
                , ("help", Just "show the help", return $ Str builtinPos "The help is a lie")]
    initial _ = return ()
