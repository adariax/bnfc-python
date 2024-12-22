{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Python3.CFtoPython3Builder (cf2Python3Builder) where

import Data.Bifunctor (Bifunctor(second))
import Data.List (intercalate, nub, intersperse)
import Data.Maybe (mapMaybe)

import BNFC.Utils ((+++), camelCase_)
import BNFC.CF
import Text.PrettyPrint.HughesPJClass (Doc, text, vcat)
import BNFC.Backend.Python3.Common
import BNFC.Options (SharedOptions (lang))
import BNFC.Backend.Antlr.CFtoAntlr4Parser (antlrRuleLabel, makeLeftRecRule)

type RuleData = (Cat, [(String, SentForm)])

joinParamsWithComma :: [String] -> String
joinParamsWithComma = intercalate ",\n"

-- getVarsFromCats :: String -> [Cat] -> [String]
-- getVarsFromCats prefix cats = zipWith (\i cat -> prefix ++ show i) [1..] cats

cf2Python3Builder :: CF -> SharedOptions -> Doc
cf2Python3Builder cf opts = vcat $
    [ importDecls
    , tokenDecls
    , buildFnDecls
    ]
  where
    language = lang opts
    importDecls = mkImportDecls cf (lang opts)
    tokenDecls = vcat $ intersperse (text "") buildTokensFuns
    buildFnDecls = vcat $ intersperse (text "") buildFuns
    buildFuns = map (mkBuildFunction language) datas
    buildTokensFuns = map mkBuildTokenFunction allTokenCats
    allTokenCats = getAllTokenCats cf
    datas = cfToGroups cf


mkImportDecls :: CF -> String -> Doc
mkImportDecls cf lang = vcat
    [
        "from typing import assert_never"
        , "from .ast import *"
        , text ctxImportStmt
        , "\n"
        ]
  where
    groups = cfToGroups cf
    ctxNames = concatMap (\(cat, rules) -> identCat cat : zipWith (\(fun, _) idx -> antlrRuleLabel cat fun (Just idx)) rules [1..]) groups
    ctxImports = intercalate ", " $ nub $ map (++ "Context") ctxNames

    tokenTypenames = getAllTokenTypenames cf
    -- typenames = nub $ tokenTypenames ++ (map (catToTsType . fst) $ filter (isUsualCat . fst) groups)
    -- astImports = intercalate ", " (filter (not . null) typenames)

    ctxImportStmt = "from " ++ parser +++ "import" +++ parser
    -- astImportStmt = "import {" ++ astImports ++ "} from './abstract'"

    parser = camelCase_ $ lang ++ "Parser"

    isUsualCat (Cat _) = True
    isUsualCat _       = False


mkThrowErrorStmt :: Cat -> Doc
mkThrowErrorStmt cat = vcat
    [ vcat (map text (indent 2 ["case _:"]))
    , vcat (map text (indent 3 ["assert_never(ctx" ++ identCat cat ++ ".op.type)"]))
    ]


mkBuildTokenFunction :: Cat -> Doc
mkBuildTokenFunction tokenCat = vcat
    [ text $ "def" +++ fnName ++ "(ctx):"
    , vcat (map text (indent 1 ["return" +++ value]))
    , "\n"
    ]
  where
    tokenName = catToStr tokenCat
    fnName = mkBuildFnName tokenCat
    value = case tokenName of
        "Integer" -> "int(ctx.INT().getText())"
        "Double"  -> "float(ctx.FLOAT().getText())"
        _         -> "ctx.getText()"


mkBuildFunction :: String -> RuleData -> Doc
mkBuildFunction lang (cat, rulesWithLabels) = vcat
    [ text $ "def " +++ mkBuildFnName cat ++ "(ctx" ++ identCat cat ++ ") -> " +++ cat2Python3Type' cat ++ ":"
    , vcat (map text (indent 1 ["match type(ctx" ++ identCat cat ++ "):"]))
    , vcat $ map (mkCaseStmt cat) datas
    , mkThrowErrorStmt cat
    , "\n"
    ]
  where
    datas = zip rulesWithLabels [1..]

    mkCaseStmt :: Cat -> ((String, SentForm), Integer) -> Doc
    mkCaseStmt catOrigin ((ruleLabel, rhsRule), ifIdx) = vcat
        [ vcat (map text (indent 2 ["case " ++ lang ++ "Parser." ++ antlrRuleLabel cat ruleLabel antlrRuleLabelIdx ++ "Context:"]))
        , vcat (map text $ concat (mkIfBody ruleLabel))
        , vcat (map text (indent 2 [""]))
        ]
      where
        antlrRuleLabelIdx = if isCoercion ruleLabel then Just ifIdx else Nothing
        rhsRuleWithIdx = mapMaybe (\(rule, idx) -> either (\cat -> Just (cat, idx)) (\_ -> Nothing) rule) $ zip rhsRule [0..]
        mkPattern idx = "expr(" ++ show idx ++ ")"
        mkIfBody ruleLabel
          | isCoercion ruleLabel =
              map (\(cat, idx) -> indent 3 ["return " +++ mkBuildFnName cat ++ "(ctx" ++ identCat catOrigin ++ "." ++ mkPattern idx ++ ")"]) rhsRuleWithIdx
          | otherwise =
              concat
              [ zipWith
                  (\(cat, idx) varName ->
                      indent 3 [varName +++ "=" +++ mkBuildFnName cat ++ "(ctx" ++ identCat catOrigin ++ "." ++ mkPattern idx ++ ")"])
                  rhsRuleWithIdx varNames
              , [indent 3 ["return " ++ antlrRuleLabel cat ruleLabel antlrRuleLabelIdx ++ "("]]
              , [[joinParamsWithComma $ indent 4 varNames]]
              , [indent 3 [")"]]
              ]
            where
              varNames = getVarsFromCats "" (map fst rhsRuleWithIdx)


cfToGroups :: CF -> [RuleData]
cfToGroups cf = map (second (map (ruleToData . makeLeftRecRule cf))) $ ruleGroups cf
  where
    ruleToData rule = ((wpThing . funRule) rule, rhsRule rule)


addParserPrefix :: String -> String -> String
addParserPrefix lang name = (upperFirst lang) ++ "Parser." ++ name