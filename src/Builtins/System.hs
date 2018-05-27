module Builtins.System where

import           Control.Monad.State (lift)
import           Data.List           (intercalate, isPrefixOf)
import           Data.Version        (versionBranch)
import           System.Info

import           Builtins.Builtin
import           Types

data SystemBuiltin = SystemBuiltin deriving (Show)

linuxCPUNames :: IO [String]
linuxCPUNames = do
  lines' <- lines <$> readFile "/proc/cpuinfo"
  pure . map (unwords . drop 3 . words) $
    filter ("model name" `isPrefixOf`) lines'

osBuiltinCpuDoc = Just "sys/cpu returns the current cpu name"
osBuiltinCpu :: Context LVal
osBuiltinCpu = Str . head <$> lift linuxCPUNames

osbuiltinCpuCoresDoc = Just "sys/cpu-cores returns the number of cores"
osBuiltinCpuCores :: Context LVal
osBuiltinCpuCores = Num . toInteger . length <$> lift linuxCPUNames

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
                , ("sys/cpu", osBuiltinCpuDoc, osBuiltinCpu)
                , ("sys/cpu-cores", osbuiltinCpuCoresDoc, osBuiltinCpuCores)
                ]
    builtins _ = []
