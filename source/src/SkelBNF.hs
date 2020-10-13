-- Haskell module generated by the BNF converter

module SkelBNF where

import qualified AbsBNF

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdentifier :: AbsBNF.Identifier -> Result
transIdentifier x = case x of
  AbsBNF.Identifier string -> failure x
transGrammar :: AbsBNF.Grammar -> Result
transGrammar x = case x of
  AbsBNF.Grammar defs -> failure x
transDef :: AbsBNF.Def -> Result
transDef x = case x of
  AbsBNF.Rule label cat items -> failure x
  AbsBNF.Comment string -> failure x
  AbsBNF.Comments string1 string2 -> failure x
  AbsBNF.Internal label cat items -> failure x
  AbsBNF.Token identifier reg -> failure x
  AbsBNF.PosToken identifier reg -> failure x
  AbsBNF.Entryp cats -> failure x
  AbsBNF.Separator minimumsize cat string -> failure x
  AbsBNF.Terminator minimumsize cat string -> failure x
  AbsBNF.Delimiters cat string1 string2 separation minimumsize -> failure x
  AbsBNF.Coercions identifier integer -> failure x
  AbsBNF.Rules identifier rhss -> failure x
  AbsBNF.Function identifier args exp -> failure x
  AbsBNF.Layout strings -> failure x
  AbsBNF.LayoutStop strings -> failure x
  AbsBNF.LayoutTop -> failure x
transItem :: AbsBNF.Item -> Result
transItem x = case x of
  AbsBNF.Terminal string -> failure x
  AbsBNF.NTerminal cat -> failure x
transCat :: AbsBNF.Cat -> Result
transCat x = case x of
  AbsBNF.ListCat cat -> failure x
  AbsBNF.IdCat identifier -> failure x
transLabel :: AbsBNF.Label -> Result
transLabel x = case x of
  AbsBNF.LabNoP labelid -> failure x
  AbsBNF.LabP labelid profitems -> failure x
  AbsBNF.LabPF labelid1 labelid2 profitems -> failure x
  AbsBNF.LabF labelid1 labelid2 -> failure x
transLabelId :: AbsBNF.LabelId -> Result
transLabelId x = case x of
  AbsBNF.Id identifier -> failure x
  AbsBNF.Wild -> failure x
  AbsBNF.ListE -> failure x
  AbsBNF.ListCons -> failure x
  AbsBNF.ListOne -> failure x
transProfItem :: AbsBNF.ProfItem -> Result
transProfItem x = case x of
  AbsBNF.ProfIt intlists integers -> failure x
transIntList :: AbsBNF.IntList -> Result
transIntList x = case x of
  AbsBNF.Ints integers -> failure x
transArg :: AbsBNF.Arg -> Result
transArg x = case x of
  AbsBNF.Arg identifier -> failure x
transSeparation :: AbsBNF.Separation -> Result
transSeparation x = case x of
  AbsBNF.SepNone -> failure x
  AbsBNF.SepTerm string -> failure x
  AbsBNF.SepSepar string -> failure x
transExp :: AbsBNF.Exp -> Result
transExp x = case x of
  AbsBNF.Cons exp1 exp2 -> failure x
  AbsBNF.App identifier exps -> failure x
  AbsBNF.Var identifier -> failure x
  AbsBNF.LitInt integer -> failure x
  AbsBNF.LitChar char -> failure x
  AbsBNF.LitString string -> failure x
  AbsBNF.LitDouble double -> failure x
  AbsBNF.List exps -> failure x
transRHS :: AbsBNF.RHS -> Result
transRHS x = case x of
  AbsBNF.RHS items -> failure x
transMinimumSize :: AbsBNF.MinimumSize -> Result
transMinimumSize x = case x of
  AbsBNF.MNonempty -> failure x
  AbsBNF.MEmpty -> failure x
transReg :: AbsBNF.Reg -> Result
transReg x = case x of
  AbsBNF.RAlt reg1 reg2 -> failure x
  AbsBNF.RMinus reg1 reg2 -> failure x
  AbsBNF.RSeq reg1 reg2 -> failure x
  AbsBNF.RStar reg -> failure x
  AbsBNF.RPlus reg -> failure x
  AbsBNF.ROpt reg -> failure x
  AbsBNF.REps -> failure x
  AbsBNF.RChar char -> failure x
  AbsBNF.RAlts string -> failure x
  AbsBNF.RSeqs string -> failure x
  AbsBNF.RDigit -> failure x
  AbsBNF.RLetter -> failure x
  AbsBNF.RUpper -> failure x
  AbsBNF.RLower -> failure x
  AbsBNF.RAny -> failure x

