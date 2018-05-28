module Builtins.Core where

import           Control.Monad.State   (lift)
import           Data.Bifoldable       (bifold)
import           Data.Foldable         (traverse_)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           Text.Megaparsec       (parse)
import           Text.Megaparsec.Error (parseErrorPretty')

import           Builtins.Builtin
import           Core                  (eval)
import           Environment           (addSymbol, addSymbolParent,
                                        emptyPartialEnv)
import qualified Parser
import           Types

data CoreBuiltin = CoreBuiltin deriving (Show)

data Scope = Global | Local

isGlobal :: Scope -> Bool
isGlobal Global = True
isGlobal _      = False

builtinLambdaDoc = Just "(\\ doc? args body)\nReturns a lambda with an optional string documentation line\n\
                        \if the args contains & and is proceeded by one final argument, all remaining arguments\n\
                        \are returned as a list with the name of that symbol."
builtinLambda :: LVal -> Context LVal
builtinLambda (SExpr xs)
    | hasDoc, (Str doc) <- head xs = return $ Lambda emptyPartialEnv (Just doc) (xs !! 1) (xs !! 2)
    | not $ isQExpr (head xs)      = return $ Err "argument list should be a QExpr"
    | not $ isQExpr (xs !! 1)      = return $ Err "body list should be a QExpr"
    | not $ checkFormals (head xs) = return $ Err "argument list should be all symbols"
    | otherwise                    = return $ Lambda emptyPartialEnv Nothing (head xs) (xs !! 1)
    where
        checkFormals (QExpr xs') = all isSym xs'
        hasDoc = length xs == 3 && isStr (head xs) && isQExpr (xs !! 1) && isQExpr (xs !! 2)

builtinVar :: Scope -> LVal -> Context LVal
builtinVar scope (SExpr xs) = do
    let (QExpr syms) = head xs
    let rest = tail xs
    let assi = zip syms rest
    if isGlobal scope then traverse_ (\ (Sym k, v) -> addSymbolParent k v) assi
    else traverse_ (\ (Sym k, v) -> addSymbol k v) assi
    return $ SExpr []

builtinDefDoc = Just "(def names values)\nGiven a list of names and a list of values def defines them in the global scope"
builtinDef :: LVal -> Context LVal
builtinDef = builtinVar Global

builtinAssDoc = Just "(= names values)\nGiven a list of names and a list of values = defines them in the local scope"
builtinAss :: LVal -> Context LVal
builtinAss = builtinVar Local

builtinEvalDoc = Just "(eval list)\nEvaluates the given list returning the result"
builtinEval :: LVal -> Context LVal
builtinEval (SExpr xs)
    | (QExpr xs') <- head xs = eval $ SExpr xs'
    | otherwise = return $ Err $ "eval handed incorrect arguments, was given: " <> tshow xs

builtinLoadDoc = Just "(load path)\nLoads the given file into the current environment"
builtinLoad :: LVal -> Context LVal
builtinLoad (SExpr xs)
    | (Str path) <- head xs = do
        contents <- lift $ readFile (T.unpack path)
        case parse Parser.exprs (T.unpack path) contents of
            Left err -> return $ Str "failed"
            Right x  -> QExpr <$> traverse eval x

builtinIfDoc = Just "(if condition truebody falsebody)\nEvaluates the given condition and if #t evalutes truebody otherwise evaluates falsebody"
builtinIf :: LVal -> Context LVal
builtinIf (SExpr xs)
    | (Boolean n) <- head xs, (QExpr tb) <- xs !! 1, (QExpr fb) <- xs !! 2 = eval $ SExpr $ if n then tb else fb
    | (QExpr [])  <- head xs, (QExpr tb) <- xs !! 1, (QExpr fb) <- xs !! 2 = eval $ SExpr fb
    | (Num n)     <- head xs, (QExpr tb) <- xs !! 1, (QExpr fb) <- xs !! 2 = eval $ SExpr $ if n >= 1 then tb else fb
    | otherwise = return $ Err $ "if handed incorrect arguments, was given: " <> tshow xs

builtinPrintlnDoc = Just "(println x & xs)\nPrints the given arguments to standard out, ending with a newline"
builtinPrintln :: LVal -> Context LVal
builtinPrintln (SExpr xs) = do
    printed <- traverse printer xs
    lift $ T.putStrLn $ bifold (T.unwords <$> sequence printed)
    return $ QExpr []

builtinPrintDoc = Just "(print x & xs)\nPrints the given arguments to standard out"
builtinPrint :: LVal -> Context LVal
builtinPrint (SExpr xs) = do
    printed <- traverse printer xs
    lift $ T.putStr $ bifold (T.unwords <$> sequence printed)
    return $ QExpr []

builtinShowDoc = Just "(show x & xs)\nShow string representation for hte supplied types"
builtinShow :: LVal -> Context LVal
builtinShow (SExpr xs) = do
    printed <- traverse printer xs
    case sequence printed of
        (Left err) -> do
            lift $ T.putStrLn err
            return $ QExpr []
        (Right x) -> return $ Str $ T.unwords x

printer :: LVal -> Context (Either Text Text)
printer (Str s)     = return $ Right s
printer (Num i)     = return $ Right (tshow i)
printer (Boolean b) = return $ Right $ if b then "#t" else "#f"
printer s@(Sym d)   = eval s >>= printer
printer e@(Err _)   = return $ Left (tshow e)
printer (QExpr xs)  = return $ Right $ tshow xs

builtinErrorDoc = Just "(error str)\nReturns an error with the value str"
builtinError :: LVal -> Context LVal
builtinError (SExpr xs)
    | (Str err) <- head xs = return $ Err err
    | otherwise = return $ Err $ "error handed incorrect arguments, recieved " <> tshow xs

instance Builtin CoreBuiltin where
    builtins _ = [ ("eval", builtinEvalDoc, builtinEval)
                 , ("load", builtinLoadDoc, builtinLoad)
                 , ("if", builtinIfDoc, builtinIf)
                 , ("\\", builtinLambdaDoc, builtinLambda)
                 , ("def", builtinDefDoc, builtinDef)
                 , ("=", builtinAssDoc, builtinAss)
                 , ("println", builtinPrintlnDoc, builtinPrintln)
                 , ("print", builtinPrintDoc, builtinPrint)
                 , ("show", builtinShowDoc, builtinShow)
                 , ("error", builtinErrorDoc, builtinError)
                 ]
    globals _ = [ ("version", Just "shows the version", return $ QExpr [Num 0, Num 0, Num 1])
                , ("help", Just "show the help", return $ Str "The help is a lie")]
    initial _ = return ()
