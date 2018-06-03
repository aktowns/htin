{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types where

import           Control.Exception               (Exception)
import           Control.Monad.Except            (ExceptT)
import           Control.Monad.State             (StateT)
import qualified Data.List.NonEmpty              as Nel
import qualified Data.Map                        as M
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import           Data.Text.Lazy                  (toStrict)
import qualified Data.Text.Lazy.Builder          as T
import           Data.Typeable                   (Typeable)
import           Data.Void                       (Void)
import           Foreign.Ptr                     (FunPtr, Ptr)
import           Formatting                      (Format, runFormat, sformat, shown, (%))
import           System.Directory                (doesFileExist)
import           System.IO                       (Handle)
import           System.Posix.DynamicLinker.Prim (DL)
import           Text.Megaparsec                 (SourcePos, initialPos, parseErrorPretty, parseErrorPretty',
                                                  sourceName)
import           Text.Megaparsec.Error           (ErrorFancy (ErrorFail), ParseError (FancyError))

import qualified Colour.TwentyFourBit            as C

data RuntimeException = RuntimeException LVal
  deriving (Typeable, Show)

instance Exception RuntimeException

type SymTab    = (M.Map Text LVal, M.Map Text LVal)
type Context a = StateT SymTab (ExceptT RuntimeException IO) a

type LBuiltin = LVal -> Context LVal
type LBuiltinVar = Context LVal

instance Show LBuiltin where show _ = "<builtin>"
instance Show LBuiltinVar where show _ = "<builtin var>"
instance Eq LBuiltin where _ == _ = False
instance Eq LBuiltinVar where _ == _ = False
instance Ord LBuiltin where compare _ _ = EQ
instance Ord LBuiltinVar where compare _ _ = EQ

data HHandle = IOHandle Handle
             | FFIHandle DL
             | FFIFunPointer (FunPtr ())
             | FFIPointer (Ptr ())

instance Show HHandle where
    show (IOHandle x)      = "IOHandle: " ++ show x
    show (FFIHandle dl)    = "FFIHandle: " ++ show dl
    show (FFIFunPointer f) = "FFIFunPointer: " ++ show f
    show (FFIPointer f)    = "FFIPointer: " ++ show f

instance Eq HHandle where
    (IOHandle x) == (IOHandle y) = x == y
    (FFIHandle _) == (FFIHandle _) = False
    _ == _ = error "unhandled hhandle equality"

instance Ord HHandle where compare _ _ = EQ

class HasPosition a where
    pos :: a -> SourcePos

instance HasPosition LVal where
    pos (Err p _)      = p
    pos (Num p _)      = p
    pos (Sym p _)      = p
    pos (Str p _)      = p
    pos (Boolean p _)  = p
    pos (Hnd p _)      = p
    pos Builtin{..}    = sourcePos
    pos BuiltinVar{..} = sourcePos
    pos Lambda{..}     = sourcePos
    pos (SExpr p _)    = p
    pos (QExpr p _)    = p

data LVal = Err        SourcePos Text
          | Num        SourcePos Integer
          | Sym        SourcePos Text
          | Str        SourcePos Text
          | Boolean    SourcePos Bool
          | Hnd        SourcePos HHandle
          | Builtin    { sourcePos :: SourcePos, doc :: Maybe Text,  fn :: LBuiltin }
          | BuiltinVar { sourcePos :: SourcePos, doc :: Maybe Text, val :: LBuiltinVar }
          | Lambda     { sourcePos :: SourcePos, partialEnv :: [(Text, LVal)], doc :: Maybe Text, formals :: LVal, body :: LVal }
          | SExpr      SourcePos [LVal]
          | QExpr      SourcePos [LVal]
          deriving (Show)

instance Ord LVal where
    compare (Err _ x) (Err _ y)         = compare x y
    compare (Num _ x) (Num _ y)         = compare x y
    compare (Sym _ x) (Sym _ y)         = compare x y
    compare (Str _ x) (Str _ y)         = compare x y
    compare (Boolean _ x) (Boolean _ y) = compare x y
    compare (Hnd _ x) (Hnd _ y)         = compare x y
    compare (SExpr _ x) (SExpr _ y)     = compare x y
    compare (QExpr _ x) (QExpr _ y)     = compare x y
    compare x y                         = error $ "order not defined for " ++ show x ++ " and " ++ show y

instance Eq LVal where
    (Err _ x) == (Err _ y) = x == y
    (Num _ x) == (Num _ y) = x == y
    (Sym _ x) == (Sym _ y) = x == y
    (Str _ x) == (Str _ y) = x == y
    (Boolean _ x) == (Boolean _ y) = x == y
    (Hnd _ x) == (Hnd _ y) = x == y
    (SExpr _ x) == (SExpr _ y) = x == y
    (QExpr _ x) == (QExpr _ y) = x == y
    x == y = error $ "equality not defined for " ++ show x ++ " and " ++ show y

toFancyError :: LVal -> ParseError Char Void
toFancyError (Err p s) = FancyError (Nel.fromList [p]) (Set.singleton $ ErrorFail (T.unpack s))
toFancyError _         = error "toFancyError called with non error"

printPrettyError' :: LVal -> IO Text
printPrettyError' e@(Err p _) = do
    let fancyErr = toFancyError e
    exists <- doesFileExist $ sourceName p
    if exists then do
        contents <- readFile $ sourceName p
        let perror = T.pack $ parseErrorPretty' contents fancyErr
        return $ C.withColour C.fgBase08 perror
    else do
        let perror = T.pack $ parseErrorPretty fancyErr
        return $ C.withColour C.fgBase08 perror
printPrettyError' _ = error "printPrettyError' called with non error"

printPrettyError :: LVal -> IO ()
printPrettyError e = printPrettyError' e >>= T.putStrLn

builtinPos :: SourcePos
builtinPos = initialPos "<builtin>"

lval :: Format r (LVal -> r)
lval = shown

tshow :: (Show a) => a -> Text
tshow = T.pack . show

isNum :: LVal -> Bool
isNum (Num _ _) = True
isNum _         = False

isSym :: LVal -> Bool
isSym (Sym _ _) = True
isSym _         = False

isStr :: LVal -> Bool
isStr (Str _ _) = True
isStr _         = False

isBoolean :: LVal -> Bool
isBoolean (Boolean _ _) = True
isBoolean _             = False

isErr :: LVal -> Bool
isErr (Err _ _) = True
isErr _         = False

isLambda :: LVal -> Bool
isLambda (Lambda{..}) = True
isLambda _            = False

isIOHandle :: LVal -> Bool
isIOHandle (Hnd _ (IOHandle _)) = True
isIOHandle _                    = False

isFFIHandle :: LVal -> Bool
isFFIHandle (Hnd _ (FFIHandle _)) = True
isFFIHandle _                     = False

isFFIPointer :: LVal -> Bool
isFFIPointer (Hnd _ (FFIPointer _)) = True
isFFIPointer _                      = False

isFFIFunPointer :: LVal -> Bool
isFFIFunPointer (Hnd _ (FFIFunPointer _)) = True
isFFIFunPointer _                         = False

isNestedErr :: LVal -> Bool
isNestedErr (Err _ _)    = True
isNestedErr (QExpr _ xs) = any isNestedErr xs
isNestedErr (SExpr _ xs) = any isNestedErr xs
isNestedErr _            = False

isSExpr :: LVal -> Bool
isSExpr (SExpr _ _) = True
isSExpr _           = False

isQExpr :: LVal -> Bool
isQExpr (QExpr _ _) = True
isQExpr _           = False

isBuiltinVar :: LVal -> Bool
isBuiltinVar BuiltinVar{..} = True
isBuiltinVar _              = False

toSExpr :: LVal -> LVal
toSExpr (QExpr c xs) = SExpr c xs
toSExpr x            = err (pos x) ("expected QExpr but got" % lval) x

unsafeToInt :: LVal -> Integer
unsafeToInt (Num _ n) = n
unsafeToInt _         = error "unsafeToInt called with non number type"

unsafeToSym :: LVal -> Text
unsafeToSym (Sym _ v) = v
unsafeToSym _         = error "unsafeToSym called with non symbol type"

unsafeToStr :: LVal -> Text
unsafeToStr (Str _ v) = v
unsafeToStr _         = error "unsafeToStr called with non string type"

unsafeToBoolean :: LVal -> Bool
unsafeToBoolean (Boolean _ v) = v
unsafeToBoolean _             = error "unsafeToBoolean called with non boolean type"

unsafeQExprList :: LVal -> [LVal]
unsafeQExprList (QExpr _ xs) = xs
unsafeQExprList _            = error "unsafeQExprList called with non qexpr type"

unsafeSExprList :: LVal -> [LVal]
unsafeSExprList (SExpr _ xs) = xs
unsafeSExprList _            = error "unsafeSExprList called with non sexpr type"

unsafeIOHandle :: LVal -> Handle
unsafeIOHandle (Hnd _ (IOHandle v)) = v
unsafeIOHandle _                    = error "unsafeIOHandle called with non IOHandle type"

unsafeFFIHandle :: LVal -> DL
unsafeFFIHandle (Hnd _ (FFIHandle h)) = h
unsafeFFIHandle _                     = error "unsafeFFIHandle called with non FFIHandle type"

unsafeFFIFunPointer :: LVal -> FunPtr ()
unsafeFFIFunPointer (Hnd _ (FFIFunPointer f)) = f
unsafeFFIFunPointer _                         = error "unsafeFFIFunPointer called with non FFIFunPointer type"

unsafeError :: RuntimeException -> Text
unsafeError (RuntimeException (Err _ v)) = v
unsafeError _                            = error "sigh"

nil :: SourcePos -> LVal
nil p = QExpr p []

fmt :: Format Text a -> a
fmt = sformat

err :: SourcePos -> Format LVal a -> a
err x m = runFormat m (Err x . toStrict . T.toLazyText)

rte :: SourcePos -> Format RuntimeException a -> a
rte x m = runFormat m (RuntimeException . Err x . toStrict . T.toLazyText)
