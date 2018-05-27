module Colour.TwentyFourBit where

import           Data.Word (Word8)

strFgTrueColour :: (Word8, Word8, Word8) -> String
strFgTrueColour (r,g,b) = "\x1B[38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

strBgTrueColour :: (Word8, Word8, Word8) -> String
strBgTrueColour (r,g,b) = "\x1B[48;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

strTrueColourReset :: String
strTrueColourReset = "\x1B[0m"

fgBase00 :: String
fgBase00 = strFgTrueColour (24, 24, 24)
bgBase00 :: String
bgBase00 = strBgTrueColour (24, 24, 24)

fgBase01 :: String
fgBase01 = strFgTrueColour (40, 40, 40)
bgBase01 :: String
bgBase01 = strBgTrueColour (40, 40, 40)

fgBase02 :: String
fgBase02 = strFgTrueColour (56, 56, 56)
bgBase02 :: String
bgBase02 = strBgTrueColour (56, 56, 56)

fgBase03 :: String
fgBase03 = strFgTrueColour (88, 88, 88)
bgBase03 :: String
bgBase03 = strBgTrueColour (88, 88, 88)

fgBase04 :: String
fgBase04 = strFgTrueColour (184, 184, 184)
bgBase04 :: String
bgBase04 = strBgTrueColour (184, 184, 184)

fgBase05 :: String
fgBase05 = strFgTrueColour (216, 216, 216)
bgBase05 :: String
bgBase05 = strBgTrueColour (216, 216, 216)

fgBase06 :: String
fgBase06 = strFgTrueColour (232, 232, 232)
bgBase06 :: String
bgBase06 = strBgTrueColour (232, 232, 232)

fgBase07 :: String
fgBase07 = strFgTrueColour (248, 248, 248)
bgBase07 :: String
bgBase07 = strBgTrueColour (248, 248, 248)

fgBase08 :: String
fgBase08 = strFgTrueColour (171, 70, 66)
bgBase08 :: String
bgBase08 = strBgTrueColour (171, 70, 66)

fgBase09 :: String
fgBase09 = strFgTrueColour (220, 150, 86)
bgBase09 :: String
bgBase09 = strBgTrueColour (220, 150, 86)

fgBase0a :: String
fgBase0a = strFgTrueColour (247, 202, 136)
bgBase0a :: String
bgBase0a = strBgTrueColour (247, 202, 136)

fgBase0b :: String
fgBase0b = strFgTrueColour (161, 181, 108)
bgBase0b :: String
bgBase0b = strBgTrueColour (161, 181, 108)

fgBase0c :: String
fgBase0c = strFgTrueColour (134, 193, 185)
bgBase0c :: String
bgBase0c = strBgTrueColour (134, 193, 185)

fgBase0d :: String
fgBase0d = strFgTrueColour (124, 175, 194)
bgBase0d :: String
bgBase0d = strBgTrueColour (124, 175, 194)

fgBase0e :: String
fgBase0e = strFgTrueColour (186, 139, 175)
bgBase0e :: String
bgBase0e = strBgTrueColour (186, 139, 175)

fgBase0f :: String
fgBase0f = strFgTrueColour (161, 105, 70)
bgBase0f :: String
bgBase0f = strBgTrueColour (161, 105, 70)
