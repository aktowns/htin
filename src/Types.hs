{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeSynonymInstances      #-}
module Types where

import           Control.Exception               (Exception, SomeException)
import           Control.Monad.Except            (ExceptT)
import           Control.Monad.State             (StateT)
import           Data.Foldable                   (traverse_)
import           Data.List                       (intercalate)
import qualified Data.List.NonEmpty              as Nel
import qualified Data.Map                        as M
import           Data.Monoid                     ((<>))
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import qualified Data.Text.Lazy.Builder          as T
import           Data.Typeable                   (Typeable)
import           Data.Void                       (Void)
import           Debug.Trace                     (traceM)
import           Foreign.Ptr                     (FunPtr, Ptr)
import           Formatting
import           System.Directory                (doesFileExist)
import           System.IO                       (Handle)
import           System.Posix.DynamicLinker.Prim (DL)
import           Text.Megaparsec                 (SourcePos, initialPos, parseErrorPretty, parseErrorPretty',
                                                  sourceName)
import           Text.Megaparsec.Error           (ErrorFancy (..), ParseError (..))

import           Colour.TwentyFourBit

data RuntimeException = RuntimeException LVal
  deriving (Typeable, Show)

instance Exception RuntimeException

type SymTab    = (M.Map Text LVal, M.Map Text LVal)
type Context a = StateT SymTab (ExceptT RuntimeException IO) a

type LBuiltin = LVal -> Context LVal
type LBuiltinVar = Context LVal

instance Show LBuiltin where show _ = "<builtin>"
instance Show LBuiltinVar where show _ = "<builtin var>"
instance Eq LBuiltin where x == y = False
instance Eq LBuiltinVar where x == y = False
instance Ord LBuiltin where compare _ _ = EQ
instance Ord LBuiltinVar where compare _ _ = EQ

data HHandle = IOHandle Handle
             | FFIHandle DL
             | forall a. FFIFunPointer (FunPtr a)
             | FFIPointer (Ptr ())

instance Show HHandle where
    show (IOHandle x)      = "IOHandle: " ++ show x
    show (FFIHandle dl)    = "FFIHandle: " ++ show dl
    show (FFIFunPointer f) = "FFIFunPointer: " ++ show f
    show (FFIPointer f)    = "FFIPointer: " ++ show f

instance Eq HHandle where
    (IOHandle x) == (IOHandle y) = x == y
    (FFIHandle x) == (FFIHandle y) = False

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
          deriving (Ord, Show)

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

printPrettyError' :: LVal -> IO Text
printPrettyError' e@(Err p _) = do
    let fancyErr = toFancyError e
    exists <- doesFileExist $ sourceName p
    if exists then do
        contents <- readFile $ sourceName p
        let perror = T.pack $ parseErrorPretty' contents fancyErr
        return $ withColour fgBase08 perror
    else do
        let perror = T.pack $ parseErrorPretty fancyErr
        return $ withColour fgBase08 perror

printPrettyError :: LVal -> IO ()
printPrettyError e = printPrettyError' e >>= T.putStrLn

builtinPos :: SourcePos
builtinPos = initialPos "<builtin>"

err :: SourcePos -> Text -> LVal
err = Err

num :: SourcePos -> Integer -> LVal
num = Num

sexpr :: SourcePos -> [LVal] -> LVal
sexpr = SExpr

qexpr :: SourcePos -> [LVal] -> LVal
qexpr = QExpr

lval :: Format r (LVal -> r)
lval = shown

tshow :: (Show a) => a -> Text
tshow = T.pack . show

showParens :: Text -> Text -> Text -> Text
showParens o c x = withColour fgBase05 o <> x <> withColour fgBase05 c

showDoc :: Maybe Text -> Text
showDoc (Just d) = withColour fgBase0c $ d <> "\n"
showDoc _        = ""

isNum :: LVal -> Bool
isNum (Num _ _) = True
isNum _         = False

isSym :: LVal -> Bool
isSym (Sym _ _) = True
isSym _         = False

isStr :: LVal -> Bool
isStr (Str _ _) = True
isStr _         = False

isErr :: LVal -> Bool
isErr (Err _ _) = True
isErr _         = False

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
toSExpr x            = err (pos x) $ sformat ("expected QExpr but got " % lval) x
