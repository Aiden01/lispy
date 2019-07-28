{-# LANGUAGE DeriveFunctor, ExistentialQuantification, DeriveAnyClass #-}
module Lispy.Parsing.AST
  ( Expr(..)
  , ExprF(..)
  , Lit(..)
  , SourceSpan(..)
  , Name
  )
where

import Data.Functor.Classes (Show1)
import Control.Comonad.Cofree
import Text.Megaparsec.Pos (SourcePos)

type Name = String

data Lit
  = LInt Integer
  | LString String
  | LChar Char
  | LFloat Double deriving (Show)

data ExprF a
  = EApp Name [a]
  | ELit Lit
  | EQuote a
  | EAtom Name
  | EList [a]
  | EIf a a 
  | ELam [Name] a deriving (Functor)

  
data SourceSpan = SourceSpan { begin, end :: SourcePos } deriving (Show)

type Expr = Cofree ExprF SourceSpan

data FnDecl = FnDecl Name [Name] Expr

newtype Program = Program [FnDecl]
