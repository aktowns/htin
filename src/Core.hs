{-# LANGUAGE RecordWildCards #-}
module Core where

import           Control.Monad.State (lift)
import           Data.Foldable       (traverse_)
import qualified Data.Text           as T
import           Debug.Trace         (traceM)
import           Formatting

import           Environment
import           Types

argParser :: [LVal] -> [LVal] -> [(LVal, LVal)]
argParser [] []                        = []
argParser formals []                   = []
argParser [formal] [arg]               = [(formal, arg)]
argParser (formal:xs) args@(arg:xs')
    | (Sym n) <- formal, n == "&"      = [(last xs, QExpr args)]
    | otherwise                        = (formal, head args) : argParser xs xs'
argParser x y                          = error $ "argParser unhandled " ++ show x ++ " vs " ++ show y

curriedArgs :: [LVal] -> [LVal] -> [LVal]
curriedArgs [] []                        = []
curriedArgs formals []                   = formals
curriedArgs [formal] [arg]               = []
curriedArgs (formal:xs) (arg:xs')
      | (Sym n) <- formal, T.head n == '&' = []
      | otherwise                        = curriedArgs xs xs'

isVarArg :: [LVal] -> Bool
isVarArg []           = False
isVarArg (Sym "&":xs) = True
isVarArg (_:xs)       = isVarArg xs

call :: LVal -> LVal -> Context LVal
call Builtin{..} args        = fn args
call Lambda{..} (SExpr args) = clonedContext $ do
    -- traceM $ "Executing lambda with args " ++ show args
    let (QExpr formals') = formals
    let curried = curriedArgs formals' args
    if not (isVarArg formals') && length args > length formals' then
        return $ Err "function given too many arguments"
    else if not (null curried) then do
        let paenv = foldl (\acc (Sym k, v) -> (k,v) : acc) partialEnv $ argParser formals' args
        return $ Lambda paenv Nothing (QExpr curried) body
    else do
        traverse_ (uncurry addSymbol) partialEnv
        traverse_ (\ (Sym k, v) -> addSymbol k v) $ argParser formals' args
        eval $ toSExpr body
call err@(Err _) _     = do
    lift $ print err
    return $ SExpr []
call (QExpr []) a      = error $ "attempted to call nil with args " ++ show a
call x a               = error $ "unhandled call target " ++ show x ++ " with args " ++ show a

eval :: LVal -> Context LVal
eval (Sym n) = do
    e <- envLookup n
    if isBuiltinVar e then let BuiltinVar{..} = e in val
    else return e
eval x@(SExpr xs) = do
    evald <- traverse eval xs
    if null evald             then return x
    else if length evald == 1 then eval $ head xs
    else call (head evald) $ SExpr (tail evald)
eval err@(Err _) = do
    lift $ print err
    return $ SExpr []
eval x = return x
