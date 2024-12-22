{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Python3.CFtoPython3Builder (cf2Python3Builder) where

import Data.Bifunctor (Bifunctor(second))
import Data.List (intercalate, intersperse)
import Data.Maybe (mapMaybe)

import BNFC.Utils ((+++), camelCase_)
import BNFC.CF
import Text.PrettyPrint.HughesPJClass (Doc, text, vcat)
import BNFC.Backend.Python3.Common
import BNFC.Options (SharedOptions (lang))
import BNFC.Backend.Antlr.CFtoAntlr4Parser (antlrRuleLabel, makeLeftRecRule)

type RuleData = (Cat, [(String, SentForm)])

cf2Python3Builder :: CF -> SharedOptions -> Doc
cf2Python3Builder cf opts = vcat $
    [ importDecls
    -- , tokenDecls
    , buildFnDecls
    ]
    
  where
    language = lang opts
    
    importDecls = mkImportDecls cf (lang opts)
    -- importDecls = [
    --     "from typing import assert_never"
    --     , "from .ast import *"
    --     , ""
    --     ]

    -- errorsDecl = buildErrors
    -- tokenDecls = vcat $ intersperse (text "") buildTokensFuns
    buildFnDecls = vcat $ intersperse (text "") buildFuns

    buildFuns = map mkBuildFunction datas
    -- buildTokensFuns = map mkBuildTokenFunction allTokenCats

    allTokenCats = getAllTokenCats cf
    datas = cfToGroups cf


mkImportDecls :: CF -> String -> Doc
mkImportDecls cf lang = vcat
    [
        "from typing import assert_never"
        , "from .ast import *"
        , ""
        , ""
        ]
  -- where
  --   groups = cfToGroups cf
  --   ctxNames = concatMap (\(cat, rules) -> identCat cat : zipWith (\(fun, _) idx -> antlrRuleLabel cat fun (Just idx)) rules [1..]) groups
  --   ctxImports = intercalate ", " $ nub $ map (++ "Context") ctxNames

  --   tokenTypenames = getAllTokenTypenames cf
  --   typenames = nub $ tokenTypenames ++ (map (catToTsType . fst) $ filter (isUsualCat . fst) groups)
  --   astImports = intercalate ", " (filter (not . null) typenames)

  --   ctxImportStmt = "import {" ++ ctxImports ++ "} from './" ++ parserFile ++ "'"
  --   astImportStmt = "import {" ++ astImports ++ "} from './abstract'"

  --   parserFile = camelCase_ $ lang ++ "Parser"

  --   isUsualCat (Cat _) = True
  --   isUsualCat _       = False


mkThrowErrorStmt :: Cat -> Doc
mkThrowErrorStmt cat = vcat [
    vcat (map text (indent 3 ["case _:"]))
    , vcat (map text (indent 4 ["assert_never(ctx" ++ (identCat cat) ++ ".op.type)"]))
    ]


mkBuildFunction :: RuleData -> Doc
mkBuildFunction (cat, rulesWithLabels) = vcat
    [ text $ "def " +++ mkBuildFnName cat ++ "(ctx" ++ identCat cat ++ ") ->" +++ cat2Python3Type' cat ++ ":"
    , vcat (map text (indent 2 ["match type(" ++ "ctx" ++ identCat cat ++ "):"]))
    , vcat $ map mkIfStmt datas
    , mkThrowErrorStmt cat
    , text $ ""
    ]
  where
    datas = zip rulesWithLabels [1..]

    mkIfStmt :: ((String, SentForm), Integer) -> Doc
    mkIfStmt ((ruleLabel, rhsRule), ifIdx) = vcat
        [ vcat (map text (indent 3 ["case" +++ "ExprParser." ++ antlrRuleLabel cat ruleLabel antlrRuleLabelIdx ++ "Context:"]))
        , vcat (map text $ concat (mkIfBody ruleLabel))
        , vcat (map text (indent 3 [""]))
        ]

      where
        antlrRuleLabelIdx = if isCoercion ruleLabel then Just ifIdx else Nothing
        rhsRuleWithIdx = mapMaybe (\(rule, idx) -> either (\cat -> Just (cat, idx)) (\_ -> Nothing) rule) $ zip rhsRule [1..]
        mkPattern idx = "expr(" ++ show idx ++ ")"

        mkIfBody ruleLabel
          | isCoercion ruleLabel = map (\(cat, idx) -> indent 4 ["return" +++ mkBuildFnName cat ++ "(ctx." ++ mkPattern idx ++ ")"]) rhsRuleWithIdx
          | isNilFun ruleLabel   = emptyListBody
          | isOneFun ruleLabel   = oneListBody
          | isConsFun ruleLabel  = consListBody
          | otherwise            =
              zipWith
              (\ (cat, idx) varName
                  -> indent 4
                      [varName
                          +++ "=" +++ mkBuildFnName cat ++ "(ctx." ++ mkPattern idx ++ ")"])
                            rhsRuleWithIdx varNames
                ++ [ indent 4 ["return None"]]
            where
              varNames = getVarsFromCats "" rhsCats
              rhsCats = map fst rhsRuleWithIdx

        emptyListBody = [indent 4 ["return []"]]
        oneListBody = map (\(cat, idx) -> indent 4 ["data =" +++ mkBuildFnName cat ++ "(arg." ++ mkPattern idx ++ ")"]) rhsRuleWithIdx ++ [ indent 4 ["return [data]"]]
        consListBody =
            [ indent 4 ["value1 =" +++  mkBuildFnName firstCat ++ "(arg." ++ mkPattern firstIdx ++ ")"]
            , indent 4 ["value2 =" +++  mkBuildFnName secondCat ++ "(arg." ++ mkPattern secondIdx ++ ")"]
            , indent 4 ["return" +++ resultList]
            ]
          where
            (firstCat, firstIdx) = head rhsRuleWithIdx
            (secondCat, secondIdx) = rhsRuleWithIdx !! 1
            (itemVar, listVar) = if isList firstCat then ("value2", "value1") else ("value1", "value2")
            resultList = if isList firstCat
              then
                "[..." ++ listVar ++ ", " ++ itemVar ++ "]"
              else
                "[" ++ itemVar ++ ", ..." ++ listVar ++ "]"


cfToGroups :: CF -> [RuleData]
cfToGroups cf = map (second (map (ruleToData . makeLeftRecRule cf))) $ ruleGroups cf
  where
    ruleToData rule = ((wpThing . funRule) rule, rhsRule rule)


addParserPrefix :: String -> String -> String
addParserPrefix lang name = (upperFirst lang) ++ "Parser." ++ name