{-# LANGUAGE QuasiQuotes #-}
module Builtins.System(SystemBuiltin(..)) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Data.Version           (versionBranch)
import           System.Info

import           Builtins.Builtin
import           Types
import           Utils.Doc

data SystemBuiltin = SystemBuiltin deriving (Show)

linuxCPUNames :: IO [Text]
linuxCPUNames = do
  lines' <- T.lines <$> T.readFile "/proc/cpuinfo"
  pure . map (T.unwords . drop 3 . T.words) $
    filter ("model name" `T.isPrefixOf`) lines'

[genDoc|sysCpu
sys/cpu

returns the current cpu name
|]
sysCpu :: Context LVal
sysCpu = (Str builtinPos) . head <$> liftIO linuxCPUNames

[genDoc|sysCpuCores
sys/cpu-cores

returns the number of cores
|]
sysCpuCores :: Context LVal
sysCpuCores = (Num builtinPos) . toInteger . length <$> liftIO linuxCPUNames

[genDoc|sysOs
sys/os

returns the current operating system
|]
sysOs :: Context LVal
sysOs = return $ Str builtinPos (T.pack os)

[genDoc|sysArch
sys/arch

returns the current architecture
|]
sysArch :: Context LVal
sysArch = return $ Str builtinPos (T.pack arch)

[genDoc|sysCompilerName
sys/compiler

returns the current compiler
|]
sysCompilerName :: Context LVal
sysCompilerName = return $ Str builtinPos (T.pack compilerName)

[genDoc|sysCompilerVersion
sys/compiler-version

returns the current compiler version
|]
sysCompilerVersion :: Context LVal
sysCompilerVersion = return $ Str builtinPos (T.intercalate "." $ map tshow $ versionBranch compilerVersion)

instance Builtin SystemBuiltin where
    initial _ = return ()
    globals _ = [ ("sys/os",               sysOsDoc,              sysOs)
                , ("sys/arch",             sysArchDoc,            sysArch)
                , ("sys/compiler",         sysCompilerNameDoc,    sysCompilerName)
                , ("sys/compiler-version", sysCompilerVersionDoc, sysCompilerVersion)
                , ("sys/cpu",              sysCpuDoc,             sysCpu)
                , ("sys/cpu-cores",        sysCpuCoresDoc,        sysCpuCores)
                ]
    builtins _ = []
