{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Python3.CFtoPython3AST (cf2Python3AST) where

import Data.Maybe      ( mapMaybe )
import BNFC.CF
import BNFC.Utils       ( (+++) )
import BNFC.Backend.Common.NamedVariables ( UserDef )
import BNFC.Backend.Python3.Common 
import Data.List (nub)

cf2Python3AST :: String -> CF -> String
cf2Python3AST langName cf = 
  let userTokens = [ n | (n,_) <- tokenPragmas cf ]
  in unlines 
    $ imports 
    ++ generateTokens userTokens
    ++ generateBaseClasses rules
    ++ concatMap astClasses rules
  where
    rules  = getAbstractSyntax cf
    imports = [ "from dataclasses import dataclass"
             ]
    censorName' = censorName langName
    str2Python3ClassName' = str2Python3ClassName langName

    generateTokens :: [UserDef] -> [String]
    generateTokens = map $ \token -> 
        let name = censorName' token 
        in name +++ "= str  # type alias"

    generateBaseClasses :: [Data] -> [String]
    generateBaseClasses rules = 
      let cats = nub $ map fst rules
          catTypes = nub $ map (stripBrackets . cat2Python3Type') cats
          prefixedTypes = map (str2Python3ClassName' . catToStr . normCat . Cat) catTypes
      in concatMap (\catType -> ["\n", "class" +++ catType ++ ": ...", ""]) prefixedTypes
      where
        stripBrackets :: String -> String
        stripBrackets = filter (\c -> c /= '[' && c /= ']')

    astClasses :: Data -> [String]
    astClasses (cat, rules) = mapMaybe (mkClassForRule cat) rules
        
    mkClassForRule :: Cat -> (String, [Cat]) -> Maybe String
    mkClassForRule cat (fun, cats)
      | isNilFun fun || 
        isOneFun fun || 
        isConsFun fun = Nothing
      | otherwise =
         Just result
      where
        caseName = str2Python3ClassName' fun
        vars = getVars langName cats
        catType = str2Python3ClassName' $ catToStr $ normCat cat
        caseAssociatedValues = map makeFieldDecl vars
        result = unlines $
          [ ""
          , "@dataclass"
          , "class" +++ caseName ++ "(" ++ catType ++ "):" ++
            if null vars 
              then " ..."
              else ""
          ] ++ 
          (if null vars
            then []
            else indent 1 caseAssociatedValues)
        
        makeFieldDecl var@((_, _), (varName, idx)) = 
          let varCat = getCatFromVar var
              normalizedCat = normCat varCat
              catStr = catToStr normalizedCat
              fieldName = if idx > 0 
                         then toSnakeCase varName ++ show idx
                         else toSnakeCase varName
              typeStr = case varCat of
                ListCat c -> "list[" ++ getTypeStr c ++ "]"
                TokenCat t -> case name2Python3BuiltIn t of
                               Just builtinType -> builtinType
                               Nothing -> stripBrackets $ cat2Python3Type' (TokenCat t)
                _ -> case name2Python3BuiltIn catStr of
                       Just builtinType -> builtinType
                       Nothing -> str2Python3ClassName' catStr
          in fieldName ++ ": " ++ typeStr
        
        stripBrackets :: String -> String
        stripBrackets = filter (\c -> c /= '[' && c /= ']')
        
        getCatFromVar :: Python3Var -> Cat
        getCatFromVar ((n, catName), _) = 
          if n > 0 
            then ListCat (Cat catName)
            else Cat catName

        getTypeStr :: Cat -> String
        getTypeStr cat = case cat of
          TokenCat t -> case name2Python3BuiltIn t of
                         Just builtinType -> builtinType
                         Nothing -> stripBrackets $ cat2Python3Type' (TokenCat t)
          _ -> let catStr = catToStr $ normCat cat
               in case name2Python3BuiltIn catStr of
                    Just builtinType -> builtinType
                    Nothing -> str2Python3ClassName' catStr
