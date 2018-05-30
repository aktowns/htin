{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types where

import           Control.Monad.State    (StateT)
import           Data.List              (intercalate)
import qualified Data.Map               as M
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.Lazy.Builder as T
import           Debug.Trace            (traceM)
import           Formatting
import           System.IO              (Handle)

import           Colour.TwentyFourBit

type SymTab    = (M.Map Text LVal, M.Map Text LVal)
type Context a = StateT SymTab IO a

type LBuiltin = LVal -> Context LVal
type LBuiltinVar = Context LVal

instance Show LBuiltin where show _ = "<builtin>"
instance Show LBuiltinVar where show _ = "<builtin var>"
instance Eq LBuiltin where x == y = False
instance Eq LBuiltinVar where x == y = False
instance Ord LBuiltin where compare _ _ = EQ
instance Ord LBuiltinVar where compare _ _ = EQ

data HHandle = IOHandle Handle
             | NetSocket
             deriving (Eq, Show)
instance Ord HHandle where compare _ _ = EQ

data LVal = Err        Text
          | Num        Integer
          | Sym        Text
          | Str        Text
          | Boolean    Bool
          | Hnd        HHandle
          | Builtin    { doc :: Maybe Text,  fn :: LBuiltin }
          | BuiltinVar { doc :: Maybe Text, val :: LBuiltinVar }
          | Lambda     { partialEnv :: [(Text, LVal)], doc :: Maybe Text, formals :: LVal, body :: LVal }
          | SExpr      [LVal]
          | QExpr      [LVal]
          deriving (Eq, Ord, Show)

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
isNum (Num _) = True
isNum _       = False

isSym :: LVal -> Bool
isSym (Sym _) = True
isSym _       = False

isStr :: LVal -> Bool
isStr (Str _) = True
isStr _       = False

isErr :: LVal -> Bool
isErr (Err _) = True
isErr _       = False

isSExpr :: LVal -> Bool
isSExpr (SExpr _) = True
isSExpr _         = False

isQExpr :: LVal -> Bool
isQExpr (QExpr _) = True
isQExpr _         = False

isBuiltinVar :: LVal -> Bool
isBuiltinVar BuiltinVar{..} = True
isBuiltinVar _              = False

toSExpr :: LVal -> LVal
toSExpr (QExpr xs) = SExpr xs
toSExpr x          = Err $ sformat ("expected QExpr but got " % lval) x

