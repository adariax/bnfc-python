{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Python3.Common where

import qualified Data.Map as Map
import BNFC.CF
import Data.Maybe
import BNFC.Utils (mkName, NameStyle (OrigCase, MixedCase), mkNames)
import qualified Data.Char as Char
import Data.Char (toLower, isUpper)

cat2Python3ClassName :: String -> Cat -> String
cat2Python3ClassName langName cat = str2Python3ClassName langName $ identCat $ normCat cat

str2Python3ClassName :: String -> String -> String
str2Python3ClassName langName str = upperFirst $ censorName langName str


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
cat2Python3Name langName cat = toList cat
  where
    toList (ListCat name) = catToStr name ++ "_list" 
    toList name = censorName langName $ catToStr name

getAllTokenCats :: CF -> [Cat]
getAllTokenCats cf = map TokenCat (literals cf)

getAllTokenTypenames :: CF -> [String]
getAllTokenTypenames cf = map cat2Python3Type' (getAllTokenCats cf)

cat2Python3Type' :: Cat -> String
cat2Python3Type' (ListCat c) = "list[" ++ cat2Python3Type' c ++ "]"
cat2Python3Type' (TokenCat c) = toMixedCase (c ++ "Token")
cat2Python3Type' cat = catToStr cat

toMixedCase :: String -> String
toMixedCase = upperFirst . mkName reservedKeywords MixedCase

name2Python3BuiltIn :: String -> Maybe String
name2Python3BuiltIn name =
  let lowerName = map toLower name
      builtInMap = [ ("integer", "int")
                   , ("int", "int")
                   , ("double", "float")
                   , ("float", "float")
                   , ("ident", "str")
                   , ("string", "str")
                   , ("char", "str")
                   ] ++ [(b, b) | b <- builtIn]
  in lookup lowerName builtInMap

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

type Python3Var = (Python3VarType, Python3VarName)

type Python3VarType = (Int, String)

type Python3VarName = (String, Int)

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
      toUnnamedVariable cat = 
        let baseType = case cat of
              ListCat c -> c
              _ -> cat
            varType = cat2Python3Type' cat
            varName = case cat of
              ListCat c -> catToStr c ++ "_list"
              _ -> cat2Python3Name' cat
        in (varType, varName)
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

buildVariableName :: Python3Var -> String
buildVariableName (_, (name, num)) = appendNumber
  where
    appendNumber 
      | num <= 0 = toSnakeCase name
      | otherwise = toSnakeCase name ++ show num

toSnakeCase :: String -> String
toSnakeCase [] = []
toSnakeCase (x:xs) = toLower x : go xs
  where
    go [] = []
    go (c:cs)
      | isUpper c = '_' : toLower c : go cs
      | otherwise = c : go cs

buildVariableType :: Python3Var -> String 
buildVariableType (vType, _) = buildVariableTypeFromPython3Type vType
  
buildVariableTypeFromPython3Type :: Python3VarType -> String
buildVariableTypeFromPython3Type vType = unpack vType
  where 
    unpack (0, name) = name
    unpack (n, name) = "list[" ++ unpack (n - 1, name) ++ "]"

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

censorName :: String -> String -> String
censorName langName name 
  | checkRegistered name = langName ++ upperFirst name
  | otherwise = name

isPythonKeyword :: String -> Bool
isPythonKeyword name = name `elem` pythonReserved

taken :: [String]
taken = [ ]

builtIn :: [String]
builtIn = [ "int"
          , "float"
          , "str"
          , "bool"
          , "set"
          , "list"
          , "tuple"
          , "dict" ]

pythonReserved :: [String]
pythonReserved = 
    [ "False"
    , "open"
    , "object"
    , "super"
    , "None" 
    , "True"
    , "and"
    , "as"
    , "assert"
    , "async"
    , "await"
    , "break"
    , "class"
    , "continue"
    , "def"
    , "del"
    , "elif"
    , "else"
    , "except"
    , "finally"
    , "for"
    , "from"
    , "global"
    , "if"
    , "import"
    , "in"
    , "is"
    , "lambda"
    , "nonlocal"
    , "not"
    , "or"
    , "pass"
    , "raise"
    , "return"
    , "try"
    , "while"
    , "with"
    , "yield"
    , "match"
    , "case"
    , "type" ]

reservedKeywords :: [String]
reservedKeywords = builtIn ++ pythonReserved