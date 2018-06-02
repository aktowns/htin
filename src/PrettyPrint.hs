{-# LANGUAGE RecordWildCards #-}
module PrettyPrint where

import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.Lazy.Builder as T
import           Formatting             (Format, later)

import qualified Colour.TwentyFourBit   as C
import           Types

class PrettyPrint a where
    pp :: a -> Text

    ppp :: a -> IO ()
    ppp = T.putStrLn . pp

instance (PrettyPrint a) => PrettyPrint [a] where
    pp a = "[" <> T.intercalate "," (map pp a) <> "]"

instance PrettyPrint LVal where
    pp (Err _ msg)    = showParens "(" ")" $ C.withColour C.fgBase08 ("error " <> msg)
    pp (Num _ i)      = C.withColour C.fgBase0d $ tshow i
    pp (Str _ s)      = C.withColour C.fgBase0b $ tshow s
    pp (Sym _ s)      = C.withColour C.fgBase09 s
    pp (Hnd _ h)      = C.withColour C.fgBase03 $ tshow h
    pp (Boolean _ b)  = C.withColour C.fgBase0e $ if b then "#t" else "#f"
    pp Builtin{..}    = showDoc doc <> C.withColour C.fgBase0a "<builtin>"
    pp BuiltinVar{..} = showDoc doc <> C.withColour C.fgBase0a "<builtin var>"
    pp (SExpr _ xs)   = showParens "(" ")" $ T.unwords $ map pp xs
    pp (QExpr _ xs)   = showParens "{" "}" $ T.unwords $ map pp xs
    pp Lambda{..}     = showDoc doc <> showParens "(" ")" (C.withColour C.fgBase09 (env partialEnv <> "\\ ") <> pp formals <> " " <> pp body)
        where
            env :: [(Text, LVal)] -> Text
            env [] = ""
            env xs = T.intercalate "," $ map (\(k,v) -> k <> ":" <> pp v) xs

ppval :: (PrettyPrint a) => Format r (a -> r)
ppval = later (T.fromText . pp)

showParens :: Text -> Text -> Text -> Text
showParens o c x = C.withColour C.fgBase05 o <> x <> C.withColour C.fgBase05 c

showDoc :: Maybe Text -> Text
showDoc (Just d) = C.withColour C.fgBase0c $ d <> "\n"
showDoc _        = ""
