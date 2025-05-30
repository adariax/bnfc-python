{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BNFC.Backend.Python3.CFtoPython3PrettyPrinter (cf2Python3PrettyPrinter) where

import Data.List (intercalate, nub, intersperse, find, partition)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Char (toLower)
import Data.Either (lefts, rights)

import BNFC.CF
import Text.PrettyPrint.HughesPJClass (Doc, text, vcat, nest)
import qualified BNFC.Backend.Python3.Common as Common
import BNFC.Options (SharedOptions(..))

cf2Python3PrettyPrinter :: CF -> String -> Doc
cf2Python3PrettyPrinter cf langName = vcat
    [ importDecls
    , rendererDeclaration
    , tokenPrintersDecl
    , nodePrintersDecl
    , nodePrettifiersDecl

    ]
  where
    importDecls = mkImportDecls cf langName
    rendererDeclaration = mkRendererClass
    tokenPrintersDecl = mkTokenPrinters cf

    cats = allParserCats cf
    
    nodePrintersDecl = vcat $ map mkNodePrinter cats
    nodePrettifiersDecl = vcat $ map (mkNodePrettifier cf langName) cats

mkImportDecls :: CF -> String -> Doc
mkImportDecls cf langName = vcat
    [ text "from typing import assert_never"
    , text ""
    , text $ "from .ast import " ++ intercalate ", " (getAllASTTypes cf langName)
    , text ""
    ]

getAllASTTypes :: CF -> String -> [String]
getAllASTTypes cf langName = 
    let rules = getAbstractSyntax cf
        baseClasses = nub $ map (Common.censorName langName . catToStr . normCat . fst) rules
        dataClasses = concatMap (getDataClassNames langName) rules
    in nub $ baseClasses ++ dataClasses
  where
    getDataClassNames :: String -> Data -> [String]
    getDataClassNames langName (cat, rules) = 
        mapMaybe (\(fun, cats) -> 
            if isNilFun fun || isOneFun fun || isConsFun fun
            then Nothing
            else Just (Common.str2Python3ClassName langName fun)) rules

mkRendererClass :: Doc
mkRendererClass = vcat
    [ text "INDENT_SIZE = 2"
    , text ""
    , text ""
    , text "def render_tokens(tokens: list[str]) -> str:"
    , nest 4 $ vcat
        [ text "transformed_tokens = transform_tokens(tokens)"
        , text "grouped_tokens = group_tokens(transformed_tokens)"
        , text "return '\\n'.join("
        , nest 4 $ text "''.join(render_token(token) for token in add_indentation(group))"
        , nest 4 $ text "for group in grouped_tokens"
        , text ")"
        ]
    , text ""
    , text ""
    , text "def render_token(token: tuple[str, str, int]) -> str:"
    , nest 4 $ vcat
        [ text "token_type, value, indent_shift = token"
        , text "match token_type:"
        , nest 4 $ vcat
            [ text "case 'text':"
            , nest 4 $ text "return value"
            , text "case 'newline':"
            , nest 4 $ text "return '\\n'"
            , text "case 'space':"
            , nest 4 $ text "return ' '"
            , text "case _:"
            , nest 4 $ text "return ''"
            ]
        ]
    , text ""
    , mkTransformFunction
    , text ""
    , mkGroupTokensFunction
    , text ""
    , mkAddIndentationFunction
    , text ""
    , mkDropTrailingFunction "spaces" "'space'"
    , text ""
    , mkDropTrailingFunction "newlines" "'newline'"
    ]

mkTransformFunction :: Doc
mkTransformFunction = vcat
    [ text ""
    , text "def transform_tokens(tokens: list[str]) -> list[tuple[str, str, int]]:"
    , nest 4 $ vcat
        [ text "result: list[tuple[str, str, int]] = []"
        , text "inside_brackets = False"
        , text "for token in tokens:"
        , nest 4 $ vcat
            [ text "match token:"
            , nest 4 $ vcat
                [ text "case '{':"
                , nest 4 $ vcat
                    [ text "result.append(('text', token, 0))"
                    , text "result.append(('newline', '', 1))"
                    ]
                , text "case '}':"
                , nest 4 $ vcat
                    [ text "drop_trailing_newlines(result)"
                    , text "result.append(('newline', '', -1))"
                    , text "result.append(('text', token, 0))"
                    , text "result.append(('newline', '', 0))"
                    ]
                , text "case '[':"
                , nest 4 $ vcat
                    [ text "drop_trailing_spaces(result)"
                    , text "result.append(('space', '', 0))"
                    , text "result.append(('text', token, 0))"
                    , text "inside_brackets = True"
                    ]
                , text "case ']':"
                , nest 4 $ vcat
                    [ text "drop_trailing_spaces(result)"
                    , text "result.append(('text', token, 0))"
                    , text "inside_brackets = False"
                    , text "result.append(('space', '', 0))"
                    ]
                , text "case ';':"
                , nest 4 $ vcat
                    [ text "drop_trailing_spaces(result)"
                    , text "drop_trailing_newlines(result)"
                    , text "result.append(('text', token, 0))"
                    , text "result.append(('newline', '', 0))"
                    ]
                , text "case 'return':"
                , nest 4 $ vcat
                    [ text "result.append(('text', token, 0))"
                    , text "result.append(('space', '', 0))"
                    ]
                , text "case '(' | ')' | '<' | '>' | ',' | '.':"
                , nest 4 $ vcat
                    [ text "drop_trailing_spaces(result)"
                    , text "if token in [')', ']', '}']:"
                    , nest 4 $ text "drop_trailing_newlines(result)"
                    , text "result.append(('text', token, 0))"
                    , text "if token not in ['.', '('] and not inside_brackets:"
                    , nest 4 $ text "result.append(('space', '', 0))"
                    ]
                , text "case _:"
                , nest 4 $ vcat
                    [ text "result.append(('text', token, 0))"
                    , text "if not inside_brackets:"
                    , nest 4 $ text "result.append(('space', '', 0))"
                    ]
                ]
            ]
        , text "drop_trailing_spaces(result)"
        , text "drop_trailing_newlines(result)"
        , text "return result"
        ]
    ]

mkGroupTokensFunction :: Doc
mkGroupTokensFunction = vcat
    [ text ""
    , text "def group_tokens(tokens: list[tuple[str, str, int]]) -> list[tuple[int, list[tuple[str, str, int]]]]:"
    , nest 4 $ vcat
        [ text "groups: list[tuple[int, list[tuple[str, str, int]]]] = []"
        , text "current_group: list[tuple[str, str, int]] = []"
        , text "current_indentation = 0"
        , text "for token in tokens:"
        , nest 4 $ vcat
            [ text "token_type, value, indent_shift = token"
            , text "match token_type:"
            , nest 4 $ vcat
                [ text "case 'newline':"
                , nest 4 $ vcat
                    [ text "current_indentation += indent_shift"
                    , text "groups.append((current_indentation, current_group))"
                    , text "current_group = []"
                    ]
                , text "case _:"
                , nest 4 $ text "current_group.append(token)"
                ]
            ]
        , text "if current_group:"
        , nest 4 $ text "groups.append((current_indentation, current_group))"
        , text "return groups"
        ]
    ]

mkAddIndentationFunction :: Doc
mkAddIndentationFunction = vcat
    [ text ""
    , text "def add_indentation(group_with_indent: tuple[int, list[tuple[str, str, int]]]) -> list[tuple[str, str, int]]:"
    , nest 4 $ vcat
        [ text "indentation_level, tokens = group_with_indent"
        , text "if indentation_level > 0:"
        , nest 4 $ vcat
            [ text "indent = ('text', ' ' * (INDENT_SIZE * indentation_level), 0)"
            , text "tokens.insert(0, indent)"
            ]
        , text "return tokens"
        ]
    ]

mkDropTrailingFunction :: String -> String -> Doc
mkDropTrailingFunction name tokenType = vcat
    [ text ""
    , text $ "def drop_trailing_" ++ name ++ "(tokens: list[tuple[str, str, int]]) -> None:"
    , nest 4 $ vcat
        [ text $ "while tokens and tokens[-1][0] == " ++ tokenType ++ ":"
        , nest 4 $ text "tokens.pop()"
        ]
    ]

mkTokenPrinters :: CF -> Doc  
mkTokenPrinters cf = vcat $ map mkTokenPrinter (literals cf)
  where
    mkTokenPrinter t = vcat
        [ text "\n"
        , text $ "def print_" ++ getTokenPrinterName (TokenCat t) ++ "(value) -> str:"
        , nest 4 $ text "return str(value)"
        ]

getTokenPrinterName :: Cat -> String
getTokenPrinterName (TokenCat t) = case t of
    "Integer" -> "integer"
    "Double" -> "double"  
    "String" -> "string"
    "Char" -> "char"
    "Ident" -> "ident"
    _ -> map toLower t
getTokenPrinterName _ = "token"

mkNodePrinter :: Cat -> Doc
mkNodePrinter cat@(Cat _) = vcat
    [ text "\n"
    , text $ "def print_" ++ catName ++ "(node: " ++ catType ++ ") -> str:"
    , nest 4 $ vcat
        [ text $ "return render_tokens(prettify_" ++ catName ++ "(node))"
        ]
    ]
  where
    catName = catToStr $ normCat cat
    catType = catName

mkNodePrinter cat@(CoercCat _ _) = vcat
    [ text "\n"
    , text $ "def print_" ++ catName ++ "(node: " ++ catType ++ ") -> str:"
    , nest 4 $ vcat
        [ text $ "return render_tokens(prettify_" ++ catName ++ "(node))"
        ]
    ]
  where
    catName = catToStr cat
    catType = catToStr $ normCat cat

mkNodePrinter listCat@(ListCat _) = vcat
    [ text "\n"
    , text $ "def print_" ++ catName ++ "(node_list: list[" ++ itemType ++ "]) -> str:"
    , nest 4 $ vcat
        [ text $ "return render_tokens(prettify_" ++ catName ++ "(node_list))"
        ]
    ]
  where
    catName = "ListOf" ++ catToStr (normCatOfList listCat)
    itemType = catToStr $ normCatOfList listCat

mkNodePrinter otherCat = text $ "# Unknown category: " ++ catToStr otherCat

mkNodePrettifier :: CF -> String -> Cat -> Doc
mkNodePrettifier cf langName cat@(Cat _) = vcat
    [ text "\n"
    , text $ "def prettify_" ++ catName ++ "(node: " ++ catType ++ ") -> list[str]:"
    , nest 4 $ vcat
        [ text "result: list[str] = []"
        , text "match node:"
        , nest 4 $ vcat $ map mkCaseStmt rules
        , nest 4 $ text "case _:"
        , nest 8 $ text "assert_never(type(node))  # type: ignore"
        ]
    ]
  where
    catName = catToStr $ normCat cat
    catType = catName
    rules = map (\rule -> (funName (funRule rule), rhsRule rule)) $
            filter (not . isCoercion . funRule) $
            rulesForNormalizedCat cf cat
    
    mkCaseStmt (ruleLabel, sentForm) = vcat
        [ text $ "case " ++ ruleLabel ++ "():"
        , nest 4 $ mkRulePrettifier langName (ruleLabel, sentForm)
        ]

mkNodePrettifier cf langName cat@(CoercCat _ _) = vcat
    [ text "\n"
    , text $ "def prettify_" ++ catName ++ "(node: " ++ catType ++ ") -> list[str]:"
    , nest 4 $ vcat
        [ text "result: list[str] = []"
        , text "match node:"
        , nest 4 $ vcat $ map mkCaseStmt myRules
        , nest 4 $ text "case _:"
        , nest 8 $ mkDefaultDelegation
        ]
    ]
  where
    catName = catToStr cat
    catType = catToStr $ normCat cat

    myRules = map (\rule -> (funName (funRule rule), rhsRule rule)) $
              filter (not . isCoercion . funRule) $ rulesForCat cf cat
    
    mkCaseStmt (ruleLabel, sentForm) = vcat
        [ text $ "case " ++ ruleLabel ++ "():"
        , nest 4 $ mkRulePrettifier langName (ruleLabel, sentForm)
        ]
    
    mkDefaultDelegation = 
        let coercionRules = filter (isCoercion . funRule) $ rulesForCat cf cat
        in case coercionRules of
            [rule] -> case rhsRule rule of
                [Left targetCat] -> 
                    let targetCatName = catToStr targetCat
                    in text $ "return prettify_" ++ targetCatName ++ "(node)"
                sentForm -> 
                    let prettifyBody = mkCoercionBody sentForm
                    in vcat $ map text prettifyBody
            _ -> text "assert_never(type(node))"
    
    mkCoercionBody :: SentForm -> [String]
    mkCoercionBody items = 
        let bodyLines = snd $ foldl mkItem (0, []) items
        in "result = []" : bodyLines ++ ["return result"]
      where
        mkItem (fieldIdx, acc) (Left targetCat) = 
            let catName = catToStr targetCat
                line = "result.extend(prettify_" ++ catName ++ "(node))"
            in (fieldIdx, acc ++ [line])
        mkItem (fieldIdx, acc) (Right terminal) = 
            (fieldIdx, acc ++ ["result.append('" ++ terminal ++ "')"])

mkNodePrettifier cf langName listCat@(ListCat _) = vcat
    [ text "\n"
    , text $ "def prettify_" ++ catName ++ "(node_list: list[" ++ itemType ++ "]) -> list[str]:"
    , nest 4 $ vcat
        [ text "result = []"
        , text "for i, item in enumerate(node_list):"
        , nest 4 $ vcat
            [ text "if i > 0:"
            , nest 4 $ text $ "result.append('" ++ separator ++ "')"
            , text $ "result.extend(prettify_" ++ itemCatName ++ "(item))"
            ]
        , text "return result"
        ]
    ]
  where
    catName = "ListOf" ++ catToStr (normCatOfList listCat)
    itemType = catToStr $ normCatOfList listCat
    itemCatName = catToStr $ normCatOfList listCat
    
    rules = rulesForCat cf listCat
    consRule = find (isConsFun . funRule) rules
    separator = fromMaybe "," $ do
        rule <- consRule
        let terminals = rights (rhsRule rule)
        case terminals of
            (sep:_) -> Just sep
            [] -> Nothing

mkNodePrettifier _ _ otherCat = text $ "# Unknown category: " ++ catToStr otherCat

mkRulePrettifier :: String -> (String, SentForm) -> Doc
mkRulePrettifier langName (ruleLabel, sentForm) = vcat
    [ text "result = []"
    , vcat $ map text $ mkPrettifyBody sentForm
    , text "return result"
    ]
  where
    fieldNames = Common.getVarsFromCats langName (lefts sentForm)
    
    mkPrettifyBody :: SentForm -> [String]
    mkPrettifyBody items = snd $ foldl mkItem (0, []) items
      where
        mkItem (fieldIdx, acc) (Left cat) 
            | isTokenCat cat = 
                let fieldName = if fieldIdx < length fieldNames then fieldNames !! fieldIdx else ""
                    line = "result.append(str(node." ++ fieldName ++ "))"
                in (fieldIdx + 1, acc ++ [line])
            | otherwise = 
                let fieldName = if fieldIdx < length fieldNames then fieldNames !! fieldIdx else ""
                    catName = case cat of
                        ListCat c -> "ListOf" ++ catToStr (normCat c)
                        _ -> catToStr cat
                    line = "result.extend(prettify_" ++ catName ++ "(node." ++ fieldName ++ "))"
                in (fieldIdx + 1, acc ++ [line])
        mkItem (fieldIdx, acc) (Right terminal) = 
            (fieldIdx, acc ++ ["result.append('" ++ terminal ++ "')"])
