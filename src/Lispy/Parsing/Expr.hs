module Lispy.Parsing.Expr
  ( expr
  )
where

import Lispy.Parsing.AST
import Lispy.Parsing.Types
import Lispy.Parsing.Lexer hiding (char)
import Control.Applicative ((<|>))
import Text.Megaparsec ((<?>))
import Text.Megaparsec.Char (space1, char, alphaNumChar)
import qualified Text.Megaparsec as Mega
import Control.Comonad.Cofree

type Parsed f = Cofree f SourceSpan

located :: Parser (f (Parsed f)) -> Parser (Parsed f)
located p = do
    s <- Mega.getSourcePos
    x <- p
    e <- Mega.getSourcePos
    pure (SourceSpan s e :< x)

lString, lInt, lFloat, lChar :: Parser Lit
lString = LString <$> stringLiteral
lInt = LInt <$> integerLiteral
lFloat = LFloat <$> floatLiteral
lChar = LChar <$> charLiteral

lit :: Parser Lit
lit =
  lChar
    <|> Mega.try lFloat
    <|> lString
    <|> lInt

spaceSep :: Parser p -> Parser [p]
spaceSep p = Mega.sepBy p space1

eLiteral :: Parser (Parsed ExprF)
eLiteral = located $ ELit <$> lit

eList :: Parser (Parsed ExprF)
eList = located $ (EList <$> (parens . spaceSep $ expr))

eQuote :: Parser (Parsed ExprF)
eQuote = located $ (EQuote <$> (char '\'' *> expr'))

eAtom :: Parser  (Parsed ExprF)
eAtom = located $ (EAtom <$> (Mega.many alphaNumChar))

eApp :: Parser (Parsed ExprF)
eApp = located app
  where app = do
          name <- identifier
          params <- Mega.sepBy expr space1
          pure $ EApp name params

expr' :: Parser (Parsed ExprF)
expr' = lexeme
  ( parens expr
    <|> eLiteral
    <|> eList
    <|> eQuote
    <|> eAtom
  )

expr :: Parser (Parsed ExprF)
expr = eApp <|> expr'
