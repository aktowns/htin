module Builtins.FFI where

import           Builtins.Builtin
import           Core                       (eval)
import           Environment
import           Types

import           Control.Exception          (SomeException, catch)
import           Control.Monad              (liftM, unless)
import           Control.Monad.Except       (throwError)
import           Control.Monad.IO.Class     (liftIO)
import           Data.ByteString            (ByteString)
import           Data.Foldable              (traverse_)
import           Data.List                  (find, genericLength)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           System.Posix.DynamicLinker (RTLDFlags (RTLD_NOW), dlopen, dlsym)
import           Text.Megaparsec            (SourcePos)

import           Data.Int
import           Data.Void                  (Void)
import           Data.Word
import           Foreign.C.Types            (CDouble, CFloat)
import           Foreign.LibFFI
import           Foreign.Ptr                (FunPtr, Ptr, castFunPtrToPtr, castPtr, castPtrToFunPtr)
import           Foreign.Storable           (peek)

data FFIBuiltin = FFIBuiltin deriving (Show)

data TypeMap = VoidTy   (RetType ())      Void
             | Int8Ty   (RetType Int8)    (Int8 -> Arg)
             | Int16Ty  (RetType Int16)   (Int16 -> Arg)
             | Int32Ty  (RetType Int32)   (Int32 -> Arg)
             | Int64Ty  (RetType Int64)   (Int64 -> Arg)
             | UInt8Ty  (RetType Word8)   (Word8 -> Arg)
             | UInt16Ty (RetType Word16)  (Word16 -> Arg)
             | UInt32Ty (RetType Word32)  (Word32 -> Arg)
             | UInt64Ty (RetType Word64)  (Word64 -> Arg)
             | FloatTy  (RetType CFloat)  (CFloat -> Arg)
             | DoubleTy (RetType CDouble) (CDouble -> Arg)
             | StringTy (RetType ByteString) (ByteString -> Arg)
             | PtrTy (RetType (Ptr ())) (Ptr () -> Arg)

instance Show TypeMap where
    show (VoidTy _ _)   = "VoidTy"
    show (Int8Ty _ _)   = "Int8Ty"
    show (Int16Ty _ _)  = "Int16Ty"
    show (Int32Ty _ _)  = "Int32Ty"
    show (Int64Ty _ _)  = "Int64Ty"
    show (UInt8Ty _ _)  = "UInt8Ty"
    show (UInt16Ty _ _) = "UInt16Ty"
    show (UInt32Ty _ _) = "UInt32Ty"
    show (UInt64Ty _ _) = "UInt64Ty"
    show (FloatTy _ _)  = "FloatTy"
    show (DoubleTy _ _) = "DoubleTy"
    show (StringTy _ _) = "StringTy"
    show (PtrTy _ _)    = "PtrTy"

typeMap = [ (1, "ffi/void",   VoidTy retVoid undefined)
          , (2, "ffi/sint8",  Int8Ty retInt8 argInt8)
          , (4, "ffi/sint16", Int16Ty retInt16 argInt16)
          , (6, "ffi/sint32", Int32Ty retInt32 argInt32)
          , (8, "ffi/sint64", Int64Ty retInt64 argInt64)
          , (3, "ffi/uint8",  UInt8Ty retWord8 argWord8)
          , (5, "ffi/uint16", UInt16Ty retWord16 argWord16)
          , (7, "ffi/uint32", UInt32Ty retWord32 argWord32)
          , (8, "ffi/uint64", UInt64Ty retWord64 argWord64)
          , (9, "ffi/float",  FloatTy  retCFloat argCFloat)
          , (10, "ffi/double", DoubleTy retCDouble argCDouble)
          , (11, "ffi/string", StringTy retMallocByteString argByteString)
          , (12, "ffi/ptr", PtrTy (retPtr retVoid) argPtr)
          ]

lookupFFITypes :: Integer -> Maybe TypeMap
lookupFFITypes n = (\(_,_,v) -> v) <$> find (\(k,_,_) -> k == n) typeMap

ffiTypes :: Context ()
ffiTypes = traverse_ (\(v,k,_) -> addSymbolParent k $ Num builtinPos v) typeMap

liftFFI :: [TypeMap] -> ([Arg] -> IO a) -> (a -> LVal) -> (LVal -> Context LVal)
liftFFI maps ffi conversion = \(SExpr p xs) ->
    if length maps /= length xs then throwError $ RuntimeException (err p "incorrect argument length")
    else do
        let argparsed = map (\(c, v) -> map2lval c v) $ zip maps xs
        out <- liftIO $ ffi argparsed
        return $ conversion out
  where map2lval (VoidTy _ _) _                        = error "void cannot be an argument"
        map2lval (Int8Ty _ c) (Num _ v)                = c $ fromInteger v
        map2lval (Int16Ty _ c) (Num _ v)               = c $ fromInteger v
        map2lval (Int32Ty _ c) (Num _ v)               = c $ fromInteger v
        map2lval (Int64Ty _ c) (Num _ v)               = c $ fromInteger v
        map2lval (UInt8Ty _ c) (Num _ v)               = c $ fromInteger v
        map2lval (UInt16Ty _ c) (Num _ v)              = c $ fromInteger v
        map2lval (UInt32Ty _ c) (Num _ v)              = c $ fromInteger v
        map2lval (UInt64Ty _ c) (Num _ v)              = c $ fromInteger v
        map2lval (StringTy _ c) (Str _ v)              = c $ T.encodeUtf8 v
        map2lval (PtrTy _ c) (Hnd _ (FFIPointer v))    = c v
        map2lval (PtrTy _ c) (Hnd _ (FFIFunPointer v)) = c $ castFunPtrToPtr v
        map2lval x y                                   = error $ "Unable to handle " ++ show x ++ " and " ++ show y

ffiDeref :: LVal -> Context LVal
ffiDeref (SExpr p xs)
    | (Hnd p (FFIPointer v)) <- head xs = do
        let ptr = castPtr v :: Ptr (Ptr a)
        ptr' <- liftIO $ peek ptr
        return $ Hnd p (FFIPointer ptr')
    | (Hnd p (FFIFunPointer v)) <- head xs = do
        let ptr = (castPtr $ castFunPtrToPtr v) :: Ptr (Ptr a)
        ptr' <- liftIO $ peek ptr
        return $ Hnd p (FFIFunPointer $ castPtrToFunPtr ptr')

ffiCall :: LVal -> Context LVal
ffiCall (SExpr p xs)
    | (Hnd p (FFIFunPointer ptr)) <- head xs, (Num _ n) <- xs !! 1, (QExpr _ xs') <- xs !! 2 = do
        let (Just ty) = lookupFFITypes n
        xs'' <- traverse eval xs'
        let args = mapM (\(Num _ n) -> lookupFFITypes n) xs''
        args' <- case args of
            Just a  -> return a
            Nothing -> throwError $ RuntimeException (err p "incorrect argument to ffi args call")
        let fn = case ty of
                (VoidTy r _)  -> Builtin { sourcePos = builtinPos, doc = Nothing, fn = liftFFI args' (callFFI ptr r) (\_ -> QExpr p []) }
                (Int8Ty r _)  -> Builtin { sourcePos = builtinPos, doc = Nothing, fn = liftFFI args' (callFFI ptr r) (\x -> Num p $ toInteger x) }
                (Int16Ty r _) -> Builtin { sourcePos = builtinPos, doc = Nothing, fn = liftFFI args' (callFFI ptr r) (\x -> Num p $ toInteger x) }
                (Int32Ty r _) -> Builtin { sourcePos = builtinPos, doc = Nothing, fn = liftFFI args' (callFFI ptr r) (\x -> Num p $ toInteger x) }
                (Int64Ty r _) -> Builtin { sourcePos = builtinPos, doc = Nothing, fn = liftFFI args' (callFFI ptr r) (\x -> Num p $ toInteger x) }
                (UInt8Ty r _) -> Builtin { sourcePos = builtinPos, doc = Nothing, fn = liftFFI args' (callFFI ptr r) (\x -> Num p $ toInteger x) }
                (UInt16Ty r _) -> Builtin { sourcePos = builtinPos, doc = Nothing, fn = liftFFI args' (callFFI ptr r) (\x -> Num p $ toInteger x) }
                (UInt32Ty r _) -> Builtin { sourcePos = builtinPos, doc = Nothing, fn = liftFFI args' (callFFI ptr r) (\x -> Num p $ toInteger x) }
                (UInt64Ty r _) -> Builtin { sourcePos = builtinPos, doc = Nothing, fn = liftFFI args' (callFFI ptr r) (\x -> Num p $ toInteger x) }
                (StringTy r _) -> Builtin { sourcePos = builtinPos, doc = Nothing, fn = liftFFI args' (callFFI ptr r) (\x -> Str p $ T.decodeUtf8 x) }
                (PtrTy r _) -> Builtin { sourcePos = builtinPos, doc = Nothing, fn = liftFFI args' (callFFI ptr r) (\x -> Hnd p $ FFIPointer x) }
        return fn

rewrap :: SourcePos -> IO LVal -> Context LVal
rewrap p f = do
    res <- liftIO $ f `catch` ((\e -> return (err p $ tshow e)) :: SomeException -> IO LVal)
    if isErr res then throwError $ RuntimeException res
    else return res

ffiDlOpen :: LVal -> Context LVal
ffiDlOpen (SExpr _ xs)
    | (Str p fp) <- head xs =
        rewrap p (Hnd p . FFIHandle <$> dlopen (T.unpack fp) [RTLD_NOW])

ffiDlSym :: LVal -> Context LVal
ffiDlSym (SExpr _ xs)
    | (Hnd p (FFIHandle h)) <- head xs, (Str _ sym) <- xs !! 1 =
        rewrap p (Hnd p . FFIFunPointer <$> dlsym h (T.unpack sym))


instance Builtin FFIBuiltin where
    builtins _ = [ ("ffi/dlopen", Just "", ffiDlOpen)
                 , ("ffi/dlsym", Just "", ffiDlSym)
                 , ("ffi/call", Just "", ffiCall)
                 , ("ffi/deref", Just "", ffiDeref)
                 ]
    globals _ = []
    initial _ = ffiTypes
