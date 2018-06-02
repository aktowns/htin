{-# LANGUAGE QuasiQuotes #-}
module Builtins.DateTime(DateTimeBuiltin(..)) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)
import           Data.Time.LocalTime

import           Builtins.Builtin
import           Types
import           Utils.Doc

data DateTimeBuiltin = DateTimeBuiltin deriving (Show)

[genDoc|datetimeCurrent
datetime/current

shows the current zoned datetime
|]
datetimeCurrent :: Context LVal
datetimeCurrent = liftIO $ (Str builtinPos) . tshow <$> getZonedTime

instance Builtin DateTimeBuiltin where
    builtins _ = []
    globals _ = [("datetime/current", datetimeCurrentDoc, datetimeCurrent)]
    initial _  = return ()
