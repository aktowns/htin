module Parser where

import           Data.Char
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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
boolLit = do
    p <- getPosition
    b <- (const True <$> symbol "#t" <|> const False <$> symbol "#f")
    return $ Boolean p b

shebang :: Parser ()
shebang = lexeme (L.skipLineComment "#!") <?> "shebang"

--comment :: Parser ()
--comment = const () <$> (char ';' >> many L.charLiteral >> char '\n')
comment :: Parser ()
comment = lexeme (L.skipLineComment ";") <?> "comment"

expr :: Parser LVal
expr = skipMany comment >> skipMany shebang >> (integer <|> stringLit <|> boolLit <|> psexpr <|> pqexpr <|> identifier)

exprs :: Parser [LVal]
exprs = many expr

integer :: Parser LVal
integer = do
    p <- getPosition
    i <- lexeme L.decimal
    return $ Num p i

stringLit :: Parser LVal
stringLit = do
    p <- getPosition
    s <- lexeme (char '"' >> manyTill pa (char '"'))
    return $ Str p $ T.pack s
    where
        pa = label "valid char literal" $ do
            notFollowedBy (char '\n')
            L.charLiteral

psexpr :: Parser LVal
psexpr = do
    p <- getPosition
    s <- parens (many expr)
    return $ SExpr p s

pqexpr :: Parser LVal
pqexpr = do
    p <- getPosition
    q <- qparens (many expr)
    return $ QExpr p q

usableInitialChars :: Parser Char
usableInitialChars =  ((satisfy (\x -> x /= ';' && isLetter x) <?> "letter")
                  <|> (satisfy (\x -> x /= ';' && isSymbol x) <?> "symbol")
                  <|> charCategory ConnectorPunctuation
                  <|> charCategory DashPunctuation
                  <|> charCategory OtherPunctuation) <?> "identifier"

usableChars :: Parser Char
usableChars = alphaNumChar <|> usableInitialChars

identifier :: Parser LVal
identifier = do
    p <- getPosition
    s <- (lexeme . try) pa
    return $ Sym p $ T.pack s
    where
        pa = (:) <$> usableInitialChars <*> many usableChars
