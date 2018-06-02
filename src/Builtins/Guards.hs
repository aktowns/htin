{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
module Builtins.Guards where

import           Control.Applicative             (Alternative (empty, (<|>)))
import           Control.Monad.Except            (throwError)
import           Data.List                       (nub)
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import           Foreign.Ptr                     (FunPtr)
import           Formatting
import           System.IO                       (Handle)
import           System.Posix.DynamicLinker.Prim (DL)
import           Text.Megaparsec                 (SourcePos)

import           Types

data Type a = Type Text (LVal -> a) (LVal -> Bool)

tyStr :: Type Text
tyStr = Type "String" unsafeToStr isStr

tyNum :: Type Integer
tyNum = Type "Number" unsafeToInt isNum

tySym :: Type Text
tySym = Type "Symbol" unsafeToSym isSym

tyQExpr :: Type [LVal]
tyQExpr = Type "QExpr" unsafeQExprList isQExpr

tyIOHandle :: Type Handle
tyIOHandle = Type "Handle" unsafeIOHandle isIOHandle

tyFFIHandle :: Type DL
tyFFIHandle = Type "FFIHandle" unsafeFFIHandle isFFIHandle

tyFFIFunPointer :: Type (FunPtr ())
tyFFIFunPointer = Type "FFIFunPointer" unsafeFFIFunPointer isFFIFunPointer

tyLambda :: Type LVal
tyLambda = Type "Lambda" id isLambda

tyBool :: Type Bool
tyBool = Type "Boolean" unsafeToBoolean isBoolean

lvalType :: LVal -> Text
lvalType (Str _ _)     = "String"
lvalType (Num _ _)     = "Number"
lvalType (Sym _ _)     = "Symbol"
lvalType (QExpr _ _)   = "QExpr"
lvalType (Boolean _ _) = "Boolean"
lvalType x             = error $ "unhandled lvaltype " <> (show x)

newtype ArgCheck a = ArgCheck { runCheck :: LVal -> (LVal, Either RuntimeException a) }

instance Functor ArgCheck where
    fmap f (ArgCheck st) = ArgCheck $ \s -> case st s of
        (s', Left errar) -> (s', Left errar)
        (s', Right a)    -> (s', Right (f a))

(^>) :: (SourcePos -> a -> b) -> ArgCheck a -> ArgCheck b
f ^> (ArgCheck st) = ArgCheck $ \s -> case st s of
        (s', Left errar) -> (s', Left errar)
        (s', Right a)    -> (s', Right (f (pos s) a))

(>^) :: ArgCheck a -> (SourcePos -> a -> b) -> ArgCheck b
(>^) = flip (^>)

(<+>) :: ArgCheck a -> ArgCheck b -> ArgCheck (a,b)
(<+>) f g = (,) <$> f <*> g

instance Applicative ArgCheck where
    pure a  = ArgCheck (\s -> (s, Right a))
    ArgCheck ff <*> ArgCheck xx = ArgCheck $ \s0 -> case ff s0 of
        (s, Left errar) -> (s, Left errar)
        (_, Right f)  -> case xx s0 of
            (s1, Left errar) -> (s1, Left errar)
            (s1, Right x)    -> (s1, Right (f x))

instance Alternative ArgCheck where
    empty = ArgCheck $ \s -> (s, Left $ RuntimeException (Err builtinPos "none"))
    ArgCheck f1 <|> ArgCheck f2 = ArgCheck $ \s0 -> case f1 s0 of
        (s1, Left _)  -> f2 s1
        (s1, Right a) -> (s1, Right a)

properCC :: ArgCheck ()
properCC = ArgCheck $ \case
    s | (SExpr _ _) <- s -> (s, Right ())
      | otherwise        -> (s, Left $ rte (pos s) "method was handed incorrect cc argument")

minParams :: Int -> ArgCheck ()
minParams n = ArgCheck $ \case
    s | (SExpr _ xs) <- s, length xs >= n -> (s, Right ())
      | (SExpr _ xs) <- s, g <- length xs ->
          (s, Left $ rte (pos s) ("expected at least " % int % " arguments, but only given " % int) n g)
      | otherwise                         -> (s, Left $ rte (pos s) ("expected at least " % int % " arguments") n)

maxParams :: Int -> ArgCheck ()
maxParams n = ArgCheck $ \case
    s | (SExpr _ xs) <- s, length xs <= n -> (s, Right ())
      | (SExpr _ xs) <- s, g <- length xs ->
          (s, Left $ rte (pos s) ("expected at most " % int % " arguments, but was given " % int) n g)
      | otherwise                         -> (s, Left $ rte (pos s) ("expected at most " % int % " arguments") n)

params :: Int -> ArgCheck ()
params n = minParams n *> maxParams n

argTypeCheck :: Int -> Type a -> ArgCheck LVal
argTypeCheck n (Type expect _ check) = ArgCheck $ \case
    s | (SExpr _ xs) <- s, n > length xs -> (s, Left $ rte (pos s) ("expected argument " % int % " to be a " % stext) n expect)
      | (SExpr _ xs) <- s, v <- xs !! (n - 1), ok <- check v, ok -> (s, Right v)
      | (SExpr _ xs) <- s, v <- xs !! (n - 1) -> (s, Left $ rte (pos v) ("expected argument " % int % " to be of type " % stext % " but was " % stext) n expect (lvalType v))
      | otherwise -> (s, Left $ rte (pos s) ("expected argument " % int % " to be of type " % stext) n expect)

argType :: Int -> Type a -> ArgCheck a
argType n t@(Type _ extract _) = argTypeCheck n t >^ \_ a -> extract a

argTypeCheckInner :: Int -> Type a -> ArgCheck LVal
argTypeCheckInner n (Type expect _ check) = ArgCheck $ \case
    s | (SExpr _ xs) <- s, n > length xs -> (s, Left $ rte (pos s) ("expected argument " % int % " to be a list of " % stext) n expect)
      | (SExpr _ xs) <- s, (Type _ ty' check') <- tyQExpr
      , list <- xs !! (n - 1)
      , ok <- check' list, ok, xs' <- ty' list
      , ok' <- all check xs', ok' -> (s, Right list)
      | (SExpr _ xs) <- s, (Type _ ty' check') <- tyQExpr
      , list <- xs !! (n - 1)
      , ok <- check' list, ok, xs' <- ty' list
      , tys <- nub (map lvalType xs') -> (s, Left $ rte (pos list) ("expected argument " % int % " to be a list of type " % stext % " but was " % shown) n expect (tshow tys))
      | (SExpr _ xs) <- s, v <- xs !! (n - 1) -> (s, Left $ rte (pos v) ("expected argument " % int % " to be a list of type " % stext % " but was " % shown) n expect (lvalType v))
      | otherwise -> (s, Left $ rte (pos s) ("expected argument " % int % " to be a list of type " % stext) n expect)

argTypeInner :: Int -> Type a -> ArgCheck [a]
argTypeInner n t@(Type _ extract _) = argTypeCheckInner n t >^ \_ xs -> map extract (unsafeQExprList xs)

allArgs :: Type a -> ArgCheck [a]
allArgs (Type expect ty check) = ArgCheck $ \case
    s | (SExpr _ xs) <- s, ok <- all check xs, ok, v <- map ty xs -> (s, Right v)
      | otherwise -> (s, Left $ rte (pos s) ("expected all arguments to be of type " % stext) expect)

args :: ArgCheck [LVal]
args = ArgCheck $ \case
    s | (SExpr _ xs) <- s -> (s, Right xs)
      | otherwise -> error "internal error: args match failed"

restArgs :: Int -> ArgCheck [LVal]
restArgs n = ArgCheck $ \case
    s | (SExpr _ xs) <- s -> (s, Right $ drop (n - 1) xs)
      | otherwise -> error "internal error: restArgs match failed"

checked :: ArgCheck a -> LVal -> Context a
checked argcheck lv = case runCheck argcheck lv of
    (_, Left errar) -> throwError errar
    (_, Right a)    -> return $ a

checked' :: ArgCheck (Context a) -> LVal -> Context a
checked' argcheck lv = case runCheck argcheck lv of
    (_, Left errar) -> throwError errar
    (_, Right a)    -> a
