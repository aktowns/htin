{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types where

import           Colour.TwentyFourBit
import           Control.Monad.State  (StateT)
import           Data.List            (intercalate)
import qualified Data.Map             as M
import           Debug.Trace          (traceM)
import           System.IO            (Handle)

type SymTab    = (M.Map String LVal, M.Map String LVal)
type Context a = StateT SymTab IO a

type LBuiltin = LVal -> Context LVal
instance Show LBuiltin where show _ = "<builtin>"
instance Eq LBuiltin where x == y = False
instance Ord LBuiltin where compare _ _ = EQ
type LBuiltinVar = Context LVal
instance Show LBuiltinVar where show _ = "<builtin var>"
instance Eq LBuiltinVar where x == y = False
instance Ord LBuiltinVar where compare _ _ = EQ

data HHandle = IOHandle Handle
             | NetSocket
             deriving (Eq, Show)
instance Ord HHandle where
    compare _ _ = EQ

data LVal = Err String
          | Num Integer
          | Sym String
          | Str String
          | Boolean Bool
          | Hnd HHandle
          | Builtin { doc :: Maybe String, fn :: LBuiltin }
          | BuiltinVar { doc :: Maybe String, val :: LBuiltinVar }
          | Lambda { partialEnv :: [(String, LVal)], doc :: Maybe String, formals :: LVal, body :: LVal }
          | SExpr [LVal]
          | QExpr [LVal]
          deriving (Eq, Ord)

showParens o c x = fgBase05 ++ o ++ strTrueColourReset ++ x ++ fgBase05 ++ c ++ strTrueColourReset

showDoc :: Maybe String -> String
showDoc (Just d) = fgBase0c ++ d ++ strTrueColourReset ++ "\n"
showDoc _        = ""

instance Show LVal where
    show (Err msg)   = showParens "(" ")" $ fgBase08 ++ "error " ++ msg ++ strTrueColourReset
    show (Num i)     = fgBase0d ++ show i ++ strTrueColourReset
    show (Str s)     = fgBase0b ++ show s ++ strTrueColourReset
    show (Sym s)     = fgBase09 ++ s ++ strTrueColourReset
    show (Hnd h)     = fgBase03 ++ show h ++ strTrueColourReset
    show (Boolean b) = fgBase0e ++ (if b then "#t" else "#f") ++ strTrueColourReset
    show Builtin{..} = showDoc doc ++ fgBase0a ++ "<builtin>" ++ strTrueColourReset
    show BuiltinVar{..} = showDoc doc ++ fgBase0a ++ "<builtin var>" ++ strTrueColourReset
    show (SExpr xs)  = showParens "(" ")" $ unwords $ map show xs
    show (QExpr xs)  = showParens "{" "}" $ unwords $ map show xs
    show Lambda{..}  =
        showDoc doc ++ showParens "(" ")" ((fgBase09 ++ env partialEnv ++ "\\ " ++ strTrueColourReset) ++ show formals ++ " " ++ show body)
        where
            env :: [(String, LVal)] -> String
            env [] = ""
            env xs = intercalate "," $ map (\(k,v) -> k ++ ":" ++ show v) xs

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
toSExpr x          = error $ "expected QExpr but got " ++ show x

