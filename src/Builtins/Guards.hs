{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
module Builtins.Guards where

import           Control.Applicative             (Alternative (..))
import           Control.Monad.Except            (throwError)
import           Data.List                       (nub)
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import           Foreign.Ptr                     (FunPtr, Ptr)
import           System.IO                       (Handle)
import           System.Posix.DynamicLinker.Prim (DL)
import           Text.Megaparsec                 (SourcePos)

import           Types

data Type a = Type Text (LVal -> a) (LVal -> Bool)

tyStr :: Type Text
tyStr = Type "String" (\(Str _ v) -> v) isStr

tyNum :: Type Integer
tyNum = Type "Number" (\(Num _ v) -> v) isNum

tySym :: Type Text
tySym = Type "Symbol" (\(Sym _ v) -> v) isSym

tyQExpr :: Type [LVal]
tyQExpr = Type "QExpr" (\(QExpr _ v) -> v) isQExpr

tyIOHandle :: Type Handle
tyIOHandle = Type "Handle" (\(Hnd _ (IOHandle h)) -> h) isIOHandle

tyFFIHandle :: Type DL
tyFFIHandle = Type "FFIHandle" (\(Hnd _ (FFIHandle h)) -> h) isFFIHandle

tyFFIFunPointer :: forall a. Type (FunPtr ())
tyFFIFunPointer = Type "FFIFunPointer" (\(Hnd _ (FFIFunPointer f)) -> f) isFFIFunPointer

tyLambda :: Type LVal
tyLambda = Type "Lambda" id isLambda

tyBool :: Type Bool
tyBool = Type "Boolean" (\(Boolean _ v) -> v) isBoolean

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
        (s, Left err) -> (s, Left err)
        (s, Right a)  -> (s, Right (f a))

(^>) :: (SourcePos -> a -> b) -> ArgCheck a -> ArgCheck b
f ^> (ArgCheck st) = ArgCheck $ \s -> case st s of
        (s, Left err) -> (s, Left err)
        (s, Right a)  -> (s, Right (f (pos s) a))

(>^) :: ArgCheck a -> (SourcePos -> a -> b) -> ArgCheck b
(>^) = flip (^>)

(<+>) :: ArgCheck a -> ArgCheck b -> ArgCheck (a,b)
(<+>) f g = (,) <$> f <*> g

instance Applicative ArgCheck where
    pure a  = ArgCheck (\s -> (s, Right a))
    ArgCheck ff <*> ArgCheck xx = ArgCheck $ \s0 -> case ff s0 of
        (s, Left err) -> (s, Left err)
        (s, Right f)  -> case xx s0 of
            (s1, Left err) -> (s1, Left err)
            (s1, Right x)  -> (s1, Right (f x))

instance Alternative ArgCheck where
    empty = ArgCheck $ \s -> (s, Left $ RuntimeException (Err builtinPos "none"))
    ArgCheck f1 <|> ArgCheck f2 = ArgCheck $ \s0 -> case f1 s0 of
        (s1, Left err) -> f2 s1
        (s1, Right a)  -> (s1, Right a)

rte :: SourcePos -> Text -> RuntimeException
rte p s = RuntimeException $ Err p s

properCC :: ArgCheck ()
properCC = ArgCheck $ \case
    s | (SExpr _ _) <- s -> (s, Right ())
      | otherwise        -> (s, Left $ rte (pos s) "method was handed incorrect cc argument")

minParams :: Int -> ArgCheck ()
minParams n = ArgCheck $ \case
    s | (SExpr _ xs) <- s, length xs >= n -> (s, Right ())
      | (SExpr _ xs) <- s, g <- length xs ->
          (s, Left $ rte (pos s) $ "expected at least " <> (tshow n) <> " arguments, but only given " <> tshow g)
      | otherwise                         -> (s, Left $ rte (pos s) $ "expected at least " <> (tshow n) <> " arguments")

maxParams :: Int -> ArgCheck ()
maxParams n = ArgCheck $ \case
    s | (SExpr _ xs) <- s, length xs <= n -> (s, Right ())
      | (SExpr _ xs) <- s, g <- length xs ->
          (s, Left $ rte (pos s) $ "expected at most " <> (tshow n) <> " arguments, but was given " <> tshow g)
      | otherwise                         -> (s, Left $ rte (pos s) $ "expected at most " <> (tshow n) <> " arguments")

params :: Int -> ArgCheck ()
params n = minParams n *> maxParams n

argTypeCheck :: Int -> Type a -> ArgCheck LVal
argTypeCheck n (Type expect _ check) = ArgCheck $ \case
    s | (SExpr _ xs) <- s, n > length xs -> (s, Left $ rte (pos s) $ "expected argument " <> (tshow n) <> " to be a " <> expect)
      | (SExpr _ xs) <- s, v <- xs !! (n - 1), ok <- check v, ok -> (s, Right v)
      | (SExpr _ xs) <- s, v <- xs !! (n - 1) -> (s, Left $ rte (pos v) $ "expected argument " <> (tshow n) <> " to be of type " <> expect <> " but was " <> (lvalType v))
      | otherwise -> (s, Left $ rte (pos s) $ "expected argument " <> (tshow n) <> " to be of type " <> expect)

argType :: Int -> Type a -> ArgCheck a
argType n t@(Type _ extract _) = argTypeCheck n t >^ \pos a -> extract a

argTypeCheckInner :: Int -> Type a -> ArgCheck LVal
argTypeCheckInner n (Type expect _ check) = ArgCheck $ \case
    s | (SExpr _ xs) <- s, n > length xs -> (s, Left $ rte (pos s) $ "expected argument " <> (tshow n) <> " to be a list of " <> expect)
      | (SExpr _ xs) <- s, (Type expect' ty' check') <- tyQExpr
      , list <- xs !! (n - 1)
      , ok <- check' list, ok, xs <- ty' list
      , ok' <- all check xs, ok' -> (s, Right list)
      | (SExpr _ xs) <- s, (Type expect' ty' check') <- tyQExpr
      , list <- xs !! (n - 1)
      , ok <- check' list, ok, xs <- ty' list
      , tys <- nub (map lvalType xs) -> (s, Left $ rte (pos list) $ "expected argument " <> (tshow n) <> " to be a list of type " <> expect <> " but was " <> (tshow tys))
      | (SExpr _ xs) <- s, v <- xs !! (n - 1) -> (s, Left $ rte (pos v) $ "expected argument " <> (tshow n) <> " to be a list of type " <> expect <> " but was " <> (lvalType v))
      | otherwise -> (s, Left $ rte (pos s) $ "expected argument " <> (tshow n) <> " to be a list of type " <> expect)

argTypeInner :: Int -> Type a -> ArgCheck [a]
argTypeInner n t@(Type _ extract _) = argTypeCheckInner n t >^ \pos (QExpr _ xs) -> map extract xs

allArgs :: Type a -> ArgCheck [a]
allArgs (Type expect ty check) = ArgCheck $ \case
    s | (SExpr _ xs) <- s, ok <- all check xs, ok, v <- map ty xs -> (s, Right v)
      | otherwise -> (s, Left $ rte (pos s) $ "expected all arguments to be of type " <> expect)

args :: ArgCheck [LVal]
args = ArgCheck $ \case
    s | (SExpr _ xs) <- s -> (s, Right xs)

restArgs :: Int -> ArgCheck [LVal]
restArgs n = ArgCheck $ \case
    s | (SExpr _ xs) <- s -> (s, Right $ drop (n - 1) xs)

checked :: ArgCheck a -> LVal -> Context a
checked argcheck lv = case runCheck argcheck lv of
    (_, Left err) -> throwError err
    (_, Right a)  -> return $ a

checked' :: ArgCheck (Context a) -> LVal -> Context a
checked' argcheck lv = case runCheck argcheck lv of
    (_, Left err) -> throwError err
    (_, Right a)  -> a
