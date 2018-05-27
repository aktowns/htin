module Builtins.DateTime where

import           Control.Monad.State (lift)
import           Data.Time.LocalTime

import           Builtins.Builtin
import           Types

data DateTimeBuiltin = DateTimeBuiltin deriving (Show)

currentDateTimeBuiltinDoc = Just "datetime/current shows the current zoned datetime"
currentDateTimeBuiltin :: Context LVal
currentDateTimeBuiltin = lift $ Str . show <$> getZonedTime

instance Builtin DateTimeBuiltin where
    builtins _ = []
    globals _ = [("datetime/current", currentDateTimeBuiltinDoc, currentDateTimeBuiltin)]
    initial _  = return ()
