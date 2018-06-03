{-# LANGUAGE RecordWildCards #-}
module Core(call, eval) where

import           Control.Monad.Except (throwError)
import           Data.Foldable        (traverse_)
import           Data.Monoid          ((<>))
import qualified Data.Text            as T

import           Environment
import           Types

call :: LVal -> LVal -> Context LVal
call Builtin{..} args        = fn args
call Lambda{..} (SExpr c args) = clonedContext $ do
    -- traceM $ "Executing lambda with args " ++ show args
    let formals' = unsafeQExprList formals
    let curried = curriedArgs formals' args
    if not (isVarArg formals') && length args > length formals' then
        throwError $ RuntimeException (Err c "function given too many arguments")
    else if not (null curried) then do
        let paenv = foldl (\acc (k, v) -> ((unsafeToSym k),v) : acc) partialEnv $ argParser formals' args
        return $ Lambda c paenv Nothing (QExpr c curried) body
    else do
        traverse_ (uncurry addSymbol) partialEnv
        traverse_ (\ (k, v) -> addSymbol (unsafeToSym k) v) $ argParser formals' args
        eval $ toSExpr body
call errar@(Err _ _) _   = throwError $ RuntimeException errar
call (QExpr p []) a    = throwError $ RuntimeException $ Err p $ "attempted to call nil with args " <> tshow a
call x a               = error $ "internal error: unhandled call target " ++ show x ++ " with args " ++ show a

eval :: LVal -> Context LVal
eval (Sym c n) = do
    e <- envLookup n
    case e of
      (Just e') | isBuiltinVar e' -> val e'
      (Just e')                   -> return e'
      _                           -> throwError $ RuntimeException (Err c $ "variable " <> n <> " is not defined, are you missing an import?")
eval x@(SExpr c xs) = do
    evald <- traverse eval xs
    if any isErr evald then throwError $ RuntimeException $ head (filter isErr evald)
    else
        if null evald             then return x
        else if length evald == 1 then eval $ head xs
        else call (head evald) $ SExpr c (tail evald)
eval errar@(Err _ _) = throwError $ RuntimeException errar
eval x = return x

argParser :: [LVal] -> [LVal] -> [(LVal, LVal)]
argParser [] []                        = []
argParser _ []                         = []
argParser [formal] [arg]               = [(formal, arg)]
argParser (formal:xs) args@(_:xs')
    | (Sym c n) <- formal, n == "&"    = [(last xs, QExpr c args)]
    | otherwise                        = (formal, head args) : argParser xs xs'
argParser x y                          = error $ "internal error: argParser unhandled " ++ show x ++ " vs " ++ show y

curriedArgs :: [LVal] -> [LVal] -> [LVal]
curriedArgs [] []                        = []
curriedArgs formals []                   = formals
curriedArgs [_] [_]                      = []
curriedArgs [] _                         = []
curriedArgs (formal:xs) (_:xs')
      | (Sym _ n) <- formal, T.head n == '&' = []
      | otherwise                            = curriedArgs xs xs'

isVarArg :: [LVal] -> Bool
isVarArg []            = False
isVarArg (Sym _ "&":_) = True
isVarArg (_:xs)        = isVarArg xs
