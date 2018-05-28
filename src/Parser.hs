module Parser where

import           Data.Char
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

import           Types

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
    where
        lineCmnt = L.skipLineComment ";"
        blockCmnt = L.skipBlockComment ";{" ";}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

qparens :: Parser a -> Parser a
qparens = between (symbol "{") (symbol "}")

boolLit :: Parser LVal
boolLit = Boolean <$> (const True <$> symbol "#t" <|> const False <$> symbol "#f")

--comment :: Parser ()
--comment = const () <$> (char ';' >> many L.charLiteral >> char '\n')
comment = lexeme (L.skipLineComment ";") <?> "comment"

expr :: Parser LVal
expr = skipMany comment >> (integer <|> stringLit <|> boolLit <|> sexpr <|> qexpr <|> identifier)

exprs :: Parser [LVal]
exprs = many expr

integer :: Parser LVal
integer = Num <$> lexeme L.decimal

stringLit :: Parser LVal
stringLit = Str . T.pack <$> lexeme (char '"' >> manyTill p (char '"'))
    where
        p = label "valid char literal" $ do
            notFollowedBy (char '\n')
            L.charLiteral

sexpr :: Parser LVal
sexpr = SExpr <$> parens (many expr)

qexpr :: Parser LVal
qexpr = QExpr <$> qparens (many expr)

usableInitialChars =  ((satisfy (\x -> x /= ';' && isLetter x) <?> "letter")
                  <|> (satisfy (\x -> x /= ';' && isSymbol x) <?> "symbol")
                  <|> charCategory ConnectorPunctuation
                  <|> charCategory DashPunctuation
                  <|> charCategory OtherPunctuation) <?> "identifier"
usableChars = alphaNumChar <|> usableInitialChars

identifier :: Parser LVal
identifier = Sym . T.pack <$> (lexeme . try) p
    where
        p = (:) <$> usableInitialChars <*> many usableChars
