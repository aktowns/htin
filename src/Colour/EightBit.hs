module Colour.EightBit where

import           Data.Word (Word8)

str256fgColour :: Word8 -> String
str256fgColour i = "\x1B[38;5;" ++ show i ++ "m"

str256bgColour :: Word8 -> String
str256bgColour i = "\x1B[48;5;" ++ show i ++ "m"

str256Reset :: String
str256Reset = "\x1B[0m"
