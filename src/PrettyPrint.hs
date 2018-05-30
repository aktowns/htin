{-# LANGUAGE RecordWildCards #-}
module PrettyPrint where

import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.Lazy.Builder as T
import           Formatting

import           Colour.TwentyFourBit
import           Types

class PrettyPrint a where
    pp :: a -> Text

    ppp :: a -> IO ()
    ppp = T.putStrLn . pp

instance (PrettyPrint a) => PrettyPrint [a] where
    pp a = "[" <> T.intercalate "," (map pp a) <> "]"

instance PrettyPrint LVal where
    pp (Err _ msg)    = showParens "(" ")" $ withColour fgBase08 ("error " <> msg)
    pp (Num _ i)      = withColour fgBase0d $ tshow i
    pp (Str _ s)      = withColour fgBase0b $ tshow s
    pp (Sym _ s)      = withColour fgBase09 s
    pp (Hnd _ h)      = withColour fgBase03 $ tshow h
    pp (Boolean _ b)  = withColour fgBase0e $ if b then "#t" else "#f"
    pp Builtin{..}    = showDoc doc <> withColour fgBase0a "<builtin>"
    pp BuiltinVar{..} = showDoc doc <> withColour fgBase0a "<builtin var>"
    pp (SExpr _ xs)   = showParens "(" ")" $ T.unwords $ map pp xs
    pp (QExpr _ xs)   = showParens "{" "}" $ T.unwords $ map pp xs
    pp Lambda{..}     = showDoc doc <> showParens "(" ")" (withColour fgBase09 (env partialEnv <> "\\ ") <> pp formals <> " " <> pp body)
        where
            env :: [(Text, LVal)] -> Text
            env [] = ""
            env xs = T.intercalate "," $ map (\(k,v) -> k <> ":" <> pp v) xs

ppval :: (PrettyPrint a) => Format r (a -> r)
ppval = later (T.fromText . pp)
