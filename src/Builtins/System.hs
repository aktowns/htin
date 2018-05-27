module Builtins.System where

import           Data.List        (intercalate)
import           Data.Version     (versionBranch)
import           System.Info

import           Builtins.Builtin
import           Types

data SystemBuiltin = SystemBuiltin deriving (Show)

osBuiltinDoc = Just "sys/os returns the current operating system"
osBuiltin :: Context LVal
osBuiltin = return $ Str os

archBuiltinDoc = Just "sys/arch returns the current architecture"
archBuiltin :: Context LVal
archBuiltin = return $ Str arch

compilerNameBuiltinDoc = Just "sys/compiler returns the current compiler"
compilerNameBuiltin :: Context LVal
compilerNameBuiltin = return $ Str compilerName

compilerVersionBuiltinDoc = Just "sys/compiler-version returns the current compiler version"
compilerVersionBuiltin :: Context LVal
compilerVersionBuiltin = return $ Str (intercalate "." $ map show $ versionBranch compilerVersion)

instance Builtin SystemBuiltin where
    initial _ = return ()
    globals _ = [ ("sys/os", osBuiltinDoc, osBuiltin)
                , ("sys/arch", archBuiltinDoc, archBuiltin)
                , ("sys/compiler", compilerNameBuiltinDoc, compilerNameBuiltin)
                , ("sys/compiler-version", compilerVersionBuiltinDoc, compilerVersionBuiltin)
                ]
    builtins _ = []
