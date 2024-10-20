{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Python3.CFtoPython3AST (cf2Python3AST) where

import Data.Maybe      ( mapMaybe )
import BNFC.CF
import BNFC.Utils       ( (+++) )
import BNFC.Backend.Common.NamedVariables ( UserDef )
import BNFC.Backend.Python3.Common 
import Data.List (intercalate, intersperse)

unwords' :: [String] -> String
unwords' = concat . intersperse ""

-- Produces abstract data types in Python3
cf2Python3AST :: String -> CF -> String
cf2Python3AST langName cf = 
  let userTokens = [ n | (n,_) <- tokenPragmas cf ]
  in unlines 
    $ imports ++ [ "" ]  -- import some libraries if needed
    ++ characterTypedef
    ++ generateTokens userTokens ++ [ "" ]
    ++ generateBaseClass ++ [ "\n" ]
    ++ concatMap astClasses rules  -- generate user-defined types
  where
    rules  = getAbstractSyntax cf
    imports = []  -- [ "from typing_extensions import assert_never" ]
    characterTypedef = [ ]
    censorName' = censorName langName
    str2Python3ClassName' = str2Python3ClassName langName
    cat2Python3ClassName' = cat2Python3ClassName langName
    getVars' = getVars langName

    generateTokens :: [UserDef] -> [String]
    generateTokens = map $ \token -> 
        let name = censorName' token 
        in "typealias" +++ name +++ "= String;"

    generateBaseClass :: [String]
    generateBaseClass = ["class Exp:" +++ "..."]
          
    -- | Generates a category class, and classes for all its rules.
    astClasses :: Data -> [String]
    astClasses (cat, rules) = categoryClass
        where
        funs = map fst rules
        cases = mapMaybe (prRule cat) rules
        categoryClass
          | catToStr cat `elem` funs || isList cat = []  -- the category is also a function or a list
          | otherwise =
            let name = cat2Python3ClassName' cat
            in [] ++ cases

    -- | Generates classes for a rule, depending on what type of rule it is.
    prRule :: Cat -> (Fun, [Cat]) -> Maybe (String)
    prRule cat (fun, cats)
      | isNilFun fun || 
        isOneFun fun || 
        isConsFun fun = Nothing  -- these are not represented in the Absyn
      | otherwise =  -- a standard rule
         Just result
      where
        caseName = str2Python3ClassName' fun
        vars = getVars' cats
        caseAssociatedValues = map (\var -> buildVariableName var ++ ": " ++ buildVariableType var) vars
        resultAssociatedValuesConcatenated
          | null vars = ""
          | otherwise = (intercalate ", " caseAssociatedValues)
        result = unwords' $ ["class" +++ caseName ++ "(Exp):" ++ "\n"]
                          ++ indent 1 ["def __init__(self," +++ resultAssociatedValuesConcatenated ++ "):", "\n" ]
                          ++ indent 2 [ "...", "\n\n" ]
