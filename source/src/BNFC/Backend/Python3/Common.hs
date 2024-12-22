{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Python3.Common where

import qualified Data.Map as Map
import BNFC.CF
import Data.Maybe
import BNFC.Utils (mkName, NameStyle (OrigCase, MixedCase), mkNames)
import qualified Data.Char as Char
import Data.Char (toLower)

cat2Python3ClassName :: String -> Cat -> String
cat2Python3ClassName langName cat = str2Python3ClassName langName $ identCat $ normCat cat

-- Pick a class name that is appropriate for the Python3
str2Python3ClassName :: String -> String -> String
str2Python3ClassName langName str = upperFirst $ censorName langName str

-- -- Pick a case name that is appropriate for the Python3
-- str2Python3CaseName :: String -> String -> String
-- str2Python3CaseName langName str = lowerFirst $ censorName langName str

-- Pick a class name that is appropriate for the Antlr
str2AntlrClassName :: String -> String
str2AntlrClassName str = upperFirst str

cat2Python3Type :: String -> Cat -> Python3VarType
cat2Python3Type langName cat = toList (0, cat)
  where
    toList :: (Int, Cat) -> Python3VarType
    toList (n, (ListCat name)) = toList (n + 1, name)
    toList (n, name) = 
      ( n
      , let n = catToStr $ normCat name
        in case (name2Python3BuiltIn n) of 
          Just bn -> bn
          Nothing -> censor n )
    censor = censorName langName

cat2Python3Name :: String -> Cat -> String
cat2Python3Name langName cat = toList $ normCat cat
  where
    toList (ListCat name) = toList name ++ "list"
    toList name = censorName langName $ catToStr name

getAllTokenCats :: CF -> [Cat]
getAllTokenCats cf = map TokenCat (literals cf)

getAllTokenTypenames :: CF -> [String]
getAllTokenTypenames cf = map cat2Python3Type' (getAllTokenCats cf)

cat2Python3Type' :: Cat -> String
cat2Python3Type' (ListCat c) = "[" ++ cat2Python3Type' c ++ "]"
cat2Python3Type' (TokenCat c) = toMixedCase (c ++ "Token")
cat2Python3Type' cat = toMixedCase (catToStr cat)

toMixedCase :: String -> String
toMixedCase = upperFirst . mkName reservedKeywords MixedCase

name2Python3BuiltIn :: String -> Maybe String
name2Python3BuiltIn name
  | name == "Integer" = Just "int"
  | name == "Double" = Just "float"
  | name == "Ident" = Just "str"
  | name == "String" = Just "str"
  | name == "Char" = Just "str"
  | otherwise = Nothing

upperFirst :: [Char] -> [Char]
upperFirst [] = []
upperFirst (letter:rest) = Char.toUpper letter : rest

lowerFirst :: [Char] -> [Char]
lowerFirst [] = []
lowerFirst (letter:rest) = Char.toLower letter : rest

indent :: Int -> [String] -> [String]
indent n lines = map addSpaces lines
  where
    addSpaces :: String -> String
    addSpaces line = (replicate (4 * n) ' ') ++ line

-- The type of an instance variable.
-- Variable type, and its name
type Python3Var = (Python3VarType, Python3VarName)

-- The type of a variable type in Python3.
-- The amount of nestings, and the underlying type name without precedence.
-- Example: List<List<Expr1>> is (2, Expr).
-- This helps to build the AST builder
type Python3VarType = (Int, String)

-- The name of a variable.
-- the name generated from the type, 
-- and the number making this variable unique
type Python3VarName = (String, Int)

-- Because of the different type representing variables, a different `getVars` is used.
getVars :: String -> [Cat] -> [Python3Var]
getVars langName cats = 
  let variables = map toUnnamedVariable cats 
      namesMap = foldl countNames Map.empty variables
      scoreMap = Map.map addScore namesMap
      (_, vars) = foldl toPython3Var (scoreMap, []) variables
  in vars
    where
      cat2Python3Name' = cat2Python3Name langName
      cat2Python3Type' = cat2Python3Type langName
      toUnnamedVariable cat = ((cat2Python3Type' cat), (cat2Python3Name' cat))
      countNames namesMap (_, name) = 
        let current = Map.findWithDefault 0 name namesMap
            next = 1 + current
        in Map.insert name next namesMap
      addScore n = (1, n)
      toPython3Var (namesMap, vars) (vType, name) =
        case (Map.lookup name namesMap) of
          Nothing -> (
            namesMap, 
            vars ++ [(vType, (name, 0))])
          Just (seen, total) -> if total <= 1 
            then (
              namesMap, 
              vars ++ [(vType, (name, 0))])
            else (
              Map.insert name (seen + 1, total) namesMap, 
              vars ++ [(vType, (name, seen))])

getVarsFromCats :: String -> [Cat] -> [String]
getVarsFromCats langName cats = mkNames ["type"] OrigCase normalizedVars
  where
    normalizedCats = map normCat cats
    indexedVars = getVars langName normalizedCats

    normalizeVar :: Python3Var -> String
    normalizeVar (_, (varName, idx)) = map toLower varName ++ varNameSuffix
      where
        varNameSuffix = if idx == 0 then "" else show idx

    normalizedVars = map normalizeVar indexedVars

mkBuildFnName :: Cat -> String
mkBuildFnName cat = "build" ++ upperFirst (restName cat)
  where
    restName cat = case cat of
      ListCat cat  -> restName cat ++ "List"
      TokenCat cat -> cat ++ "Token"
      otherCat     -> catToStr otherCat

-- From a Python3Var build its string representation
buildVariableName :: Python3Var -> String
buildVariableName (_, (name, num)) = lowerFirst appendNumber
  where
    appendNumber 
      | num <= 0 = name
      | otherwise = name ++ show num

-- From a Python3Var make a name for the AST
buildVariableType :: Python3Var -> String 
buildVariableType (vType, _) = buildVariableTypeFromPython3Type vType
  
buildVariableTypeFromPython3Type :: Python3VarType -> String
buildVariableTypeFromPython3Type vType = unpack vType
  where 
    unpack (0, name) = name
    unpack (n, name) = "[" ++ unpack (n - 1, name) ++ "]"

checkBuiltIn :: String -> Bool
checkBuiltIn name = 
  (lowerFirst name) `elem` concatMap 
      (map lowerFirst) 
      [ builtIn, pythonReserved ]

checkRegistered :: String -> Bool
checkRegistered name = 
  (lowerFirst name) `elem` concatMap 
      (map lowerFirst) 
      [ builtIn, pythonReserved, taken ]

-- Prevent some type or variable name to be called as some already used type or keyword
censorName :: String -> String -> String
censorName langName name 
  | checkRegistered name = langName ++ upperFirst name
  | otherwise = name


taken = [ ]
builtIn = [ "int"
          , "float"
          , "str"
          , "bool"
          , "set"
          , "tuple"
          , "dict" ]

pythonReserved :: [String]
pythonReserved = 
    [ "False"
    , "await"
    , "else"
    , "import"
    , "pass"
    , "None"
    , "break"
    , "except"
    , "in"
    , "raise"
    , "True"
    , "class"
    , "finally"
    , "is"
    , "return"
    , "and"
    , "continue"
    , "for"
    , "lambda"
    , "try"
    , "as"
    , "def"
    , "from"
    , "nonlocal"
    , "while"
    , "assert"
    , "del"
    , "global"
    , "not"
    , "with"
    , "async"
    , "elif"
    , "if"
    , "or"
    , "yield"
    , "match"
    , "case"
    , "type" ]

reservedKeywords :: [String]
reservedKeywords = builtIn ++ pythonReserved