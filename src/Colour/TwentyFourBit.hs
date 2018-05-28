{-# LANGUAGE OverloadedStrings #-}
module Colour.TwentyFourBit( fgBase00, bgBase00, fgBase01, bgBase01, fgBase02, bgBase02,
                             fgBase03, bgBase03, fgBase04, bgBase04, fgBase05, bgBase05,
                             fgBase06, bgBase06, fgBase07, bgBase07, fgBase08, bgBase08,
                             fgBase09, bgBase09, fgBase0a, bgBase0a, fgBase0b, bgBase0b,
                             fgBase0c, bgBase0c, fgBase0d, bgBase0d, fgBase0e, bgBase0e,
                             fgBase0f, bgBase0f, trueColourReset, bgTrueColour, fgTrueColour,
                             withColour
                            ) where

import           Data.Text (Text)
import qualified Data.Text as Text

import           Data.Word (Word8)

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

fgTrueColour :: (Word8, Word8, Word8) -> Text
fgTrueColour (r,g,b) = "\x1B[38;2;" <> tshow r <> ";" <> tshow g <> ";" <> tshow b <> "m"

bgTrueColour :: (Word8, Word8, Word8) -> Text
bgTrueColour (r,g,b) = "\x1B[48;2;" <> tshow r <> ";" <> tshow g <> ";" <> tshow b <> "m"

withColour :: Text -> Text -> Text
withColour c f = c <> f <> trueColourReset

trueColourReset :: Text
trueColourReset = "\x1B[0m"

fgBase00 :: Text
fgBase00 = fgTrueColour (24, 24, 24)
bgBase00 :: Text
bgBase00 = bgTrueColour (24, 24, 24)

fgBase01 :: Text
fgBase01 = fgTrueColour (40, 40, 40)
bgBase01 :: Text
bgBase01 = bgTrueColour (40, 40, 40)

fgBase02 :: Text
fgBase02 = fgTrueColour (56, 56, 56)
bgBase02 :: Text
bgBase02 = bgTrueColour (56, 56, 56)

fgBase03 :: Text
fgBase03 = fgTrueColour (88, 88, 88)
bgBase03 :: Text
bgBase03 = bgTrueColour (88, 88, 88)

fgBase04 :: Text
fgBase04 = fgTrueColour (184, 184, 184)
bgBase04 :: Text
bgBase04 = bgTrueColour (184, 184, 184)

fgBase05 :: Text
fgBase05 = fgTrueColour (216, 216, 216)
bgBase05 :: Text
bgBase05 = bgTrueColour (216, 216, 216)

fgBase06 :: Text
fgBase06 = fgTrueColour (232, 232, 232)
bgBase06 :: Text
bgBase06 = bgTrueColour (232, 232, 232)

fgBase07 :: Text
fgBase07 = fgTrueColour (248, 248, 248)
bgBase07 :: Text
bgBase07 = bgTrueColour (248, 248, 248)

fgBase08 :: Text
fgBase08 = fgTrueColour (171, 70, 66)
bgBase08 :: Text
bgBase08 = bgTrueColour (171, 70, 66)

fgBase09 :: Text
fgBase09 = fgTrueColour (220, 150, 86)
bgBase09 :: Text
bgBase09 = bgTrueColour (220, 150, 86)

fgBase0a :: Text
fgBase0a = fgTrueColour (247, 202, 136)
bgBase0a :: Text
bgBase0a = bgTrueColour (247, 202, 136)

fgBase0b :: Text
fgBase0b = fgTrueColour (161, 181, 108)
bgBase0b :: Text
bgBase0b = bgTrueColour (161, 181, 108)

fgBase0c :: Text
fgBase0c = fgTrueColour (134, 193, 185)
bgBase0c :: Text
bgBase0c = bgTrueColour (134, 193, 185)

fgBase0d :: Text
fgBase0d = fgTrueColour (124, 175, 194)
bgBase0d :: Text
bgBase0d = bgTrueColour (124, 175, 194)

fgBase0e :: Text
fgBase0e = fgTrueColour (186, 139, 175)
bgBase0e :: Text
bgBase0e = bgTrueColour (186, 139, 175)

fgBase0f :: Text
fgBase0f = fgTrueColour (161, 105, 70)
bgBase0f :: Text
bgBase0f = bgTrueColour (161, 105, 70)



