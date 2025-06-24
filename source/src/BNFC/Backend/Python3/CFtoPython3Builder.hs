{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Python3.CFtoPython3Builder (cf2Python3Builder) where

import Data.Bifunctor (Bifunctor(second))
import Data.List (intercalate, nub, isPrefixOf)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Char (toLower, isDigit)
import Data.Either (isRight)

import BNFC.Utils ((+++), camelCase_)
import BNFC.CF
import Text.PrettyPrint.HughesPJClass (Doc, text, vcat, empty, (<>))
import BNFC.Backend.Python3.Common
import BNFC.Backend.Common.NamedVariables (firstLowerCase)
import BNFC.Options (SharedOptions (..))
import BNFC.Backend.Antlr.CFtoAntlr4Parser (antlrRuleLabel)

type RuleData = (Cat, [(String, [(Cat, Int)])])

getCorrectTypeName :: String -> Cat -> String
getCorrectTypeName langName cat = 
  let catStr = catToStr (normCat cat)
  in str2Python3ClassName langName catStr

cf2Python3Builder :: CF -> String -> Doc
cf2Python3Builder cf langName = vcat
    [ importDecls
    , vcat $ map mkTokenBuilder (literals cf)
    , vcat $ mapMaybe (mkBuildFunction langName cf) (cfToGroups cf)
    , entrypoint
    , text ""
    ]
  where
    importDecls = mkImportDecls cf langName
    entrypoint = mkBuildEntrypointFunction langName cf

    mkTokenBuilder :: String -> Doc
    mkTokenBuilder t = 
      let name = getTokenBuilderName t
          conv = case t of
                  "Integer" -> "int"
                  "Double" -> "float"
                  "String" -> "str"
                  "Char" -> "str"
                  "Ident" -> "str"
                  _ -> "str"
          antlrTokenType = "TerminalNodeImpl"
          body = ["return " ++ conv ++ "(ctx.getText())"]
      in text $ unlines $ ["\n", "def " ++ name ++ "(ctx: " ++ antlrTokenType ++ ") -> " ++ conv ++ ":"] ++ indent 1 body

mkImportDecls :: CF -> String -> Doc
mkImportDecls cf langName = vcat
    [ text "from typing import assert_never"
    , text "from antlr4 import CommonTokenStream"
    , text "from antlr4.tree.Tree import TerminalNodeImpl"
    , text ""
    , astImport
    , text $ "from ." ++ parser ++ " import " ++ parser
    , text $ "from ." ++ lexer ++ " import " ++ lexer
    ]
  where
    parser = camelCase_ $ langName ++ "Parser"
    lexer = camelCase_ $ langName ++ "Lexer"
    astTypes = nub $ collectASTTypes cf langName
    astImport = if null astTypes
                then text "# No AST types to import"
                else text $ "from .ast import " ++ intercalate ", " astTypes

collectASTTypes :: CF -> String -> [String]
collectASTTypes cf langName = 
  let rules = getAbstractSyntax cf
      cats = nub $ map fst rules
      -- Check if there are any dataclasses (non-list rules)
      hasDataClasses = any hasNonListRules rules
      -- Collect all categories that appear in the grammar (including in lists)
      allUsedCats = nub $ concatMap getAllCatsFromData rules ++ cats
      -- Include only language-specific type aliases for tokens actually defined in the grammar
      -- We skip builtin types like Integer/Double since they may not be defined as type aliases
      -- even when used implicitly in the grammar
      languageTypeAliases = mapMaybe (\token ->
        if token `notElem` ["Integer", "Double", "String", "Char", "Ident"] &&
           any (\c -> case c of
                       TokenCat t -> t == token
                       _ -> False) allUsedCats
        then Just token  -- Use the token name directly, not prefixed with langName
        else Nothing) (literals cf)
      typeAliases = languageTypeAliases
      -- Only collect valid non-list categories for base classes
      baseClasses = mapMaybe (\cat -> 
        case cat of
          ListCat _ -> Nothing  -- Skip list categories
          TokenCat _ -> Nothing  -- Skip token categories, they are handled as type aliases
          _ -> let catStr = catToStr (normCat cat)
               in if catStr `elem` ["Integer", "Double", "String", "Char", "Ident"]
                  then Nothing  -- These are handled as type aliases
                  else Just (str2Python3ClassName langName catStr)) cats
      dataClasses = concatMap (getDataClassNames langName) rules
      validTypes = nub $ typeAliases ++ baseClasses ++ dataClasses
  in validTypes
  where
    hasNonListRules (_, ruleList) = any (\(fun, _) -> not (isNilFun fun || isOneFun fun || isConsFun fun)) ruleList
    
    getAllCatsFromData :: Data -> [Cat]
    getAllCatsFromData (cat, rules) = cat : concatMap (\(_, cats) -> cats) rules
    
    getDataClassNames :: String -> Data -> [String]
    getDataClassNames langName (cat, rules) = 
      mapMaybe (\(fun, cats) -> 
        if isNilFun fun || isOneFun fun || isConsFun fun
        then Nothing
        else Just (str2Python3ClassName langName fun)) rules

mkBuildFunction :: String -> CF -> RuleData -> Maybe Doc
mkBuildFunction langName cf (cat, rules) = 
  if null rules || isTokenCat cat
     then Nothing
     else Just $ vcat $ map text $
       ["\n", "def build" ++ fnName ++ "(ctx: " ++ contextType ++ ") -> " ++ retType ++ ":"] ++
       indent 1 ["match type(ctx):"] ++
       concatMap (mkCaseStmt langName cf cat) (zip rules [1..]) ++
       indent 2 ["case _:"] ++
       indent 3 ["assert_never(type(ctx))  # type: ignore"]
  where
    fnName = case cat of
      ListCat c -> "List" ++ catToStr c
      _ -> catToStr cat
    
    retType = case cat of
      ListCat c -> "list[" ++ getCorrectTypeName langName c ++ "]"
      _ -> getCorrectTypeName langName cat
    
    contextType = camelCase_ langName ++ "Parser." ++ cleanCatName cat ++ "Context"
      where
        cleanCatName (ListCat c) = "List" ++ catToStr c
        cleanCatName c = catToStr c

mkCaseStmt :: String -> CF -> Cat -> ((String, [(Cat, Int)]), Integer) -> [String]
mkCaseStmt langName cf cat ((fun, catsWithIndices), idx) = 
  let contextName = antlrRuleLabel cat fun (if isCoercion fun then Just idx else Nothing)
      parser = camelCase_ langName ++ "Parser"
      mkPattern i = "children[" ++ show i ++ "]"
  in indent 2 ["case " ++ parser ++ "." ++ contextName ++ "Context:"] ++
     concat (mkIfBody langName fun catsWithIndices mkPattern)

mkIfBody :: String -> String -> [(Cat, Int)] -> (Int -> String) -> [[String]]
mkIfBody langName fun catsWithIndices mkPattern
  | isNilFun fun = [indent 3 ["return []"]]
  | isOneFun fun =
      let (nextCat, idx) = head catsWithIndices
          buildCall = mkBuildCall nextCat ++ "(ctx." ++ mkPattern idx ++ ")"
      in [indent 3 ["return [" ++ buildCall ++ "]"]]
  | isConsFun fun =
      let [(cat1, idx1), (cat2, idx2)] = catsWithIndices
          (restCat, restIdx, elemCat, elemIdx) = 
            case (cat1, cat2) of
              (ListCat _, _) -> (cat1, idx1, cat2, idx2)
              (_, ListCat _) -> (cat2, idx2, cat1, idx1)
              _ -> (cat1, idx1, cat2, idx2)
          buildRestList = mkBuildCall restCat ++ "(ctx." ++ mkPattern restIdx ++ ")"
          buildElement = mkBuildCall elemCat ++ "(ctx." ++ mkPattern elemIdx ++ ")"
      in [indent 3 ["element = " ++ buildElement,
                   "rest_list = " ++ buildRestList,
                   "return [element] + rest_list"]]
  | isCoercion fun =
      let (nextCat, idx) = head catsWithIndices
          buildCall = "return " ++ mkBuildCall nextCat ++ "(ctx." ++ mkPattern idx ++ ")"
      in [indent 3 [buildCall]]
  | otherwise =
      let cats = map fst catsWithIndices
          varNames = getVarsFromCats langName cats
          buildLines = zipWith 
            (\(c, i) var -> 
              concat [var, " = ", mkBuildCall c, "(ctx.", mkPattern i, ")"])
            catsWithIndices
            varNames
          returnLine = concat ["return ", str2Python3ClassName langName fun, "(", intercalate ", " varNames, ")"]
      in [indent 3 buildLines, indent 3 [returnLine]]

mkBuildCall :: Cat -> String
mkBuildCall cat = case cat of
  ListCat c -> "buildList" ++ catToStr c
  TokenCat t -> getTokenBuilderName t
  c -> "build" ++ catToStr c

mkBuildEntrypointFunction :: String -> CF -> Doc
mkBuildEntrypointFunction langName cf = vcat $ map text $
    ["\n", "def build(input_stream) -> " ++ returnType ++ ":"] ++
    indent 1
        [ "lexer = " ++ lexer ++ "(input_stream)"
        , "stream = CommonTokenStream(lexer)"
        , "parser = " ++ parser ++ "(stream)"
        , ""
        , "return build" ++ catType ++ "(parser.start_" ++ catType ++ "()." ++ methodName ++ "())"
        ]
  where
    groups = cfToGroups cf
    cat = fst (head groups)
    catType = case cat of
      ListCat c -> "List" ++ catToStr c
      _ -> catToStr cat
    methodName = case cat of
      ListCat c -> "list" ++ catToStr c  -- ANTLR generates method names like listInteger
      _ -> map toLower (catToStr cat)
    returnType = case cat of
      ListCat c -> "list[" ++ getCorrectTypeName langName c ++ "]"
      _ -> getCorrectTypeName langName cat
    parser = camelCase_ $ langName ++ "Parser"
    lexer = camelCase_ $ langName ++ "Lexer"

cfToGroups :: CF -> [RuleData]
cfToGroups cf = map (second (map ruleToData)) $ ruleGroups cf
  where
    ruleToData :: Rule -> (String, [(Cat, Int)])
    ruleToData rule = (funName (funRule rule), 
                      mapMaybe (\(item, idx) -> case item of 
                                                Left cat -> Just (cat, idx)
                                                Right _ -> Nothing) 
                              (zip (rhsRule rule) [0..]))


getTokenBuilderName :: String -> String
getTokenBuilderName t = "build" ++ case t of
    "Integer" -> "int"
    "Double" -> "dbl"
    "String" -> "str"
    "Char" -> "chr"
    "Ident" -> "id"
    _ -> map toLower (take 3 t)
