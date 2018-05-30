{-# LANGUAGE RecordWildCards #-}
module Core where

import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable          (traverse_)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Debug.Trace            (traceM)
import           Formatting

import           Environment
import           Types

argParser :: [LVal] -> [LVal] -> [(LVal, LVal)]
argParser [] []                        = []
argParser formals []                   = []
argParser [formal] [arg]               = [(formal, arg)]
argParser (formal:xs) args@(arg:xs')
    | (Sym c n) <- formal, n == "&"    = [(last xs, QExpr c args)]
    | otherwise                        = (formal, head args) : argParser xs xs'
argParser x y                          = error $ "internal error: argParser unhandled " ++ show x ++ " vs " ++ show y

curriedArgs :: [LVal] -> [LVal] -> [LVal]
curriedArgs [] []                        = []
curriedArgs formals []                   = formals
curriedArgs [formal] [arg]               = []
curriedArgs (formal:xs) (arg:xs')
      | (Sym _ n) <- formal, T.head n == '&' = []
      | otherwise                            = curriedArgs xs xs'

isVarArg :: [LVal] -> Bool
isVarArg []             = False
isVarArg (Sym _ "&":xs) = True
isVarArg (_:xs)         = isVarArg xs

call :: LVal -> LVal -> Context LVal
call Builtin{..} args        = fn args
call Lambda{..} (SExpr c args) = clonedContext $ do
    -- traceM $ "Executing lambda with args " ++ show args
    let (QExpr _ formals') = formals
    let curried = curriedArgs formals' args
    if not (isVarArg formals') && length args > length formals' then
        throwError $ RuntimeException (err c "function given too many arguments")
    else if not (null curried) then do
        let paenv = foldl (\acc (Sym _ k, v) -> (k,v) : acc) partialEnv $ argParser formals' args
        return $ Lambda c paenv Nothing (QExpr c curried) body
    else do
        traverse_ (uncurry addSymbol) partialEnv
        traverse_ (\ (Sym _ k, v) -> addSymbol k v) $ argParser formals' args
        eval $ toSExpr body
call err@(Err _ _) _   = throwError $ RuntimeException err
call (QExpr p []) a    = throwError $ RuntimeException $ err p $ "attempted to call nil with args " <> tshow a
call x a               = error $ "internal error: unhandled call target " ++ show x ++ " with args " ++ show a

eval :: LVal -> Context LVal
eval (Sym c n) = do
    e <- envLookup n
    case e of
      (Just e') | isBuiltinVar e' -> val e'
      (Just e')                   -> return e'
      otherwise                   -> throwError $ RuntimeException (err c $ "variable " <> n <> " is not defined, are you missing an import?")
eval x@(SExpr c xs) = do
    evald <- traverse eval xs
    if null evald             then return x
    else if length evald == 1 then eval $ head xs
    else call (head evald) $ SExpr c (tail evald)
eval err@(Err _ _) = throwError $ RuntimeException err
eval x = return x
