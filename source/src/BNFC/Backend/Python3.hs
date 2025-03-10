{-# LANGUAGE RecordWildCards #-}

module BNFC.Backend.Python3 ( makePython3 ) where

import Text.PrettyPrint ( text, vcat, render, nest )

import Prelude hiding ((<>))
import System.FilePath ((</>), pathSeparator)
import System.Directory ( createDirectoryIfMissing )
import Data.Char (toLower)

import BNFC.Backend.Base (MkFiles, mkfile,liftIO)
import BNFC.CF (CF, getAbstractSyntax, firstEntry, catToStr, identCat, normCat )
import BNFC.Options (SharedOptions (Options, inPackage, lang, optMake, dLanguage, antlrOpts, outDir), AntlrTarget (Python3))
import BNFC.Utils (mkName, NameStyle (SnakeCase, CamelCase), replace, (+.+), (+++))
import BNFC.Backend.Common.Makefile as MakeFile 
import BNFC.Backend.Common.NamedVariables (firstLowerCase) 
import BNFC.Backend.Antlr (makeAntlr, makeAntlr', DirectoryOptions (DirectoryOptions, baseDirectory, nameStyle))

import BNFC.Backend.Python3.CFtoPython3AST ( cf2Python3AST )
import BNFC.Backend.Python3.CFtoPython3Builder ( cf2Python3Builder )
import BNFC.Backend.Python3.Common ( indent, buildVariableTypeFromPython3Type, cat2Python3ClassName, upperFirst )

makePython3 :: SharedOptions -> CF -> MkFiles ()
makePython3 opts@Options{..} cf = do
    let dirBase = replace '.' pathSeparator $ packageName
        langBase = dirBase
        libLang = langBase
        srcLang = libLang
        
        -- Generates files in an incorrect place
    makeAntlr (opts {dLanguage = Python3, optMake = Nothing}) cf
    MakeFile.mkMakefile optMake $ makefileContent dirBase


    mkfile (srcLang </> "ast.py") makePython3Comment astContent
    mkfile (srcLang </> "builder.py") makePython3Comment builderContent
  
  where
    astContent = cf2Python3AST (firstLowerCase langName) cf
    builderContent = cf2Python3Builder cf opts
    packageName = maybe id (+.+) inPackage $ mkName [] CamelCase lang
    langName = mkName [] CamelCase lang
    -- langNameUpperCased = firstUpperCase langName
    importLangName = "import 'package:" ++ langName ++ "_generated/" ++ langName ++ "_generated.Python3';"

    lexerClassName = lang ++ "GrammarLexer"
    parserClassName = lang ++ "GrammarParser"

    makeVars x = [MakeFile.mkVar n v | (n,v) <- x]
    makeRules x = [MakeFile.mkRule tar dep recipe  | (tar, dep, recipe) <- x]
    makefileVars = vcat $ makeVars
      [("LANG", langName)
      , ("LEXER_NAME", upperFirst langName ++ "Lexer")
      , ("PARSER_NAME", upperFirst langName ++ "Parser")
      , ("ANTLR4", "antlr4") -- installed using pip
      ]
    refVarInSrc dirBase refVar = dirBase </> MakeFile.refVar refVar
    rmFile :: (String -> String) -> String -> String -> String
    rmFile refSrcVar refVar ext = "rm -f" +++ refSrcVar refVar ++ ext
    makefileRules refSrcVar = 
      let rmInSrc = rmFile refSrcVar
      in vcat $ makeRules
        [ (".PHONY", ["all", "clean", "remove"], [])
        , ("all", [MakeFile.refVar "LANG"], [])
        , ("lexer"
            , [refSrcVar "LEXER_NAME" ++ ".g4"]
            , [MakeFile.refVar "ANTLR4" +++ "-Dlanguage=Python3" +++ refSrcVar "LEXER_NAME" ++ ".g4"])
        , ("parser"
            , [refSrcVar "PARSER_NAME" ++ ".g4"]
            , [MakeFile.refVar "ANTLR4" +++ "-Dlanguage=Python3" +++ "-no-listener" +++ "-no-visitor" +++ refSrcVar "PARSER_NAME" ++ ".g4"])
        , (MakeFile.refVar "LANG", ["lexer", "parser", "clean"], [])
        , ("clean", [],
          [ 
            rmInSrc "LEXER_NAME" ".interp"
          , rmInSrc "LEXER_NAME" ".tokens"
          , rmInSrc "PARSER_NAME" ".interp"
          , rmInSrc "PARSER_NAME" ".tokens"
          , rmInSrc "LEXER_NAME" ".g4"
          , rmInSrc "PARSER_NAME" ".g4"
          ])
        , ("remove", [], ["rm -rf" +++ MakeFile.refVar "LANG"])
        ]
    makefileContent dirBase _ = vcat [makefileVars, "", makefileRules $ refVarInSrc dirBase, ""]

makePython3Comment :: String -> String
makePython3Comment = ("# Python3 " ++)

makePython3CommentYaml :: String -> String
makePython3CommentYaml = ("# Python3" ++)

toLowerCase :: String -> String
toLowerCase = map toLower