module Builtins.DateTime where

import           Control.Monad.IO.Class (liftIO)
import           Data.Time.LocalTime

import           Builtins.Builtin
import           Types

data DateTimeBuiltin = DateTimeBuiltin deriving (Show)

currentDateTimeBuiltinDoc = Just "datetime/current shows the current zoned datetime"
currentDateTimeBuiltin :: Context LVal
currentDateTimeBuiltin = liftIO $ (Str builtinPos) . tshow <$> getZonedTime

instance Builtin DateTimeBuiltin where
    builtins _ = []
    globals _ = [("datetime/current", currentDateTimeBuiltinDoc, currentDateTimeBuiltin)]
    initial _  = return ()
