module Builtins.System where

import           Control.Monad.IO.Class (liftIO)
import           Data.List              (intercalate, isPrefixOf)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Data.Version           (versionBranch)
import           System.Info

import           Builtins.Builtin
import           Types

data SystemBuiltin = SystemBuiltin deriving (Show)

linuxCPUNames :: IO [Text]
linuxCPUNames = do
  lines' <- T.lines <$> T.readFile "/proc/cpuinfo"
  pure . map (T.unwords . drop 3 . T.words) $
    filter ("model name" `T.isPrefixOf`) lines'

osBuiltinCpuDoc = Just "sys/cpu returns the current cpu name"
osBuiltinCpu :: Context LVal
osBuiltinCpu = (Str builtinPos) . head <$> liftIO linuxCPUNames

osbuiltinCpuCoresDoc = Just "sys/cpu-cores returns the number of cores"
osBuiltinCpuCores :: Context LVal
osBuiltinCpuCores = (Num builtinPos) . toInteger . length <$> liftIO linuxCPUNames

osBuiltinDoc = Just "sys/os returns the current operating system"
osBuiltin :: Context LVal
osBuiltin = return $ Str builtinPos (T.pack os)

archBuiltinDoc = Just "sys/arch returns the current architecture"
archBuiltin :: Context LVal
archBuiltin = return $ Str builtinPos (T.pack arch)

compilerNameBuiltinDoc = Just "sys/compiler returns the current compiler"
compilerNameBuiltin :: Context LVal
compilerNameBuiltin = return $ Str builtinPos (T.pack compilerName)

compilerVersionBuiltinDoc = Just "sys/compiler-version returns the current compiler version"
compilerVersionBuiltin :: Context LVal
compilerVersionBuiltin = return $ Str builtinPos (T.intercalate "." $ map tshow $ versionBranch compilerVersion)

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
