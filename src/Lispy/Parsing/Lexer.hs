module Lispy.Parsing.Lexer
  ( lexeme
  , space
  , symbol
  , keywords
  , identifier
  , char
  , parens
  , braces
  , brackets
  , commaSep
  , colon
  , charLiteral
  , stringLiteral
  , integerLiteral
  , floatLiteral
  )
where

import qualified Text.Megaparsec               as Mega
import Text.Megaparsec ((<|>), (<?>))
import qualified Text.Megaparsec.Char          as MegaC
import qualified Text.Megaparsec.Char.Lexer    as MegaL
import Lispy.Parsing.Types
import qualified Data.Set as S

lexeme :: Parser a -> Parser a
lexeme = MegaL.lexeme space

space, skipLineComment, skipBlockComment :: Parser ()
space = MegaL.space MegaC.space1 skipLineComment skipBlockComment
skipLineComment = MegaL.skipLineComment ";;"
skipBlockComment = MegaL.skipBlockComment "(*" "*)"

symbol :: String -> Parser String
symbol = MegaL.symbol space

keywords :: S.Set String
keywords = S.fromList ["def"]


identifier :: Parser String
identifier = (lexeme . Mega.try $ p >>= check) <?> "identifier"
  where
    p = (:) <$> MegaC.lowerChar <*> Mega.many (MegaC.alphaNumChar <|> MegaC.digitChar <|> Mega.oneOf ("'_"))
    check x = if x `S.member` keywords
              then fail $ "Keyword “" <> x <> "” used as identifier."
              else pure x

char :: Char -> Parser Char
char = lexeme . MegaC.char

parens, braces, brackets :: Parser a -> Parser a
-- Parses what's inside the parentheses
parens = Mega.between (char '(') (char ')')
-- Parses what's inside braces
braces = Mega.between (char '{') (char '}')
-- Parses what's inside brackets
brackets = Mega.between (char '[') (char ']')

commaSep :: Parser a -> Parser [a]
commaSep p = Mega.sepBy p (symbol ",")

colon :: Parser String
colon = lexeme (symbol ":")

charLiteral :: Parser Char
charLiteral = MegaC.char '\'' *> MegaL.charLiteral <* MegaC.char '\''

stringLiteral :: Parser String
stringLiteral = MegaC.char '"' >> Mega.manyTill p (MegaC.char '"')
  where
    p = Mega.label "valid string literal" $ do
        Mega.notFollowedBy (MegaC.char '\n')
        MegaL.charLiteral

integerLiteral :: Parser Integer
integerLiteral = MegaL.decimal

floatLiteral :: Parser Double
floatLiteral = MegaL.float
