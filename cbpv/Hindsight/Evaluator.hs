https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module Hindsight.Evaluator where

import Data.List
import qualified MinHS.Env as E
import Hindsight.Syntax
import MinHS.Syntax(Op(..),TyCon(..))

import MinHS.Pretty(datacon,numeric,ANSIPretty(..))
import Prettyprinter as PP
import Prettyprinter.Render.Terminal

instance {-# OVERLAPPING #-} ANSIPretty Value where
  ansipp (I i) = numeric $ i
  ansipp (B b) = datacon $ show b
  ansipp (Nil) = datacon "Nil"
  ansipp (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> ansipp v)
  ansipp _ = undefined -- should not ever be used

instance Pretty Value where
  pretty = unAnnotate . ansipp

type VEnv = E.Env Value

evaluate :: CBind -> Value
evaluate (CBind _ _ _ e) =
  case evalC E.empty e of
    P v -> v
    _   -> error "Top-level computation produced non-ground value"

--- From here is where you should put your work!

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           -- TODO: others as needed

data Terminal =
           P Value
           -- TODO: others as needed

evalV :: VEnv -> VExp -> Value
evalV _ _ = error "TODO: implement evalV"
evalC :: VEnv -> CExp -> Terminal
evalC _ _ = error "TODO: implement evalC"
