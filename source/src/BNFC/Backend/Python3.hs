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
import BNFC.Backend.Python3.CFtoPython3PrettyPrinter ( cf2Python3PrettyPrinter )
import BNFC.Backend.Python3.Common ( indent, buildVariableTypeFromPython3Type, cat2Python3ClassName, upperFirst )

makePython3 :: SharedOptions -> CF -> MkFiles ()
makePython3 opts@Options{..} cf = do
    let dirBase = replace '.' pathSeparator $ packageName
        langBase = dirBase
        libLang = langBase
        srcLang = libLang

    makeAntlr (opts {dLanguage = Python3, optMake = Nothing}) cf
    MakeFile.mkMakefile optMake $ makefileContent dirBase

    mkfile (srcLang </> "__init__.py") makePython3Comment ("" :: String)
    mkfile (srcLang </> "ast.py") makePython3Comment astContent
    mkfile (srcLang </> "builder.py") makePython3Comment builderContent
    mkfile (srcLang </> "printer.py") makePython3Comment printerContent
    mkfile ("requirements.txt") makePython3Comment requirementsContent

    MakeFile.mkMakefile optMake $ makefileContent dirBase

  where
    astContent = cf2Python3AST (firstLowerCase langName) cf
    builderContent = render $ cf2Python3Builder cf (firstLowerCase langName)
    printerContent = render $ cf2Python3PrettyPrinter cf (firstLowerCase langName)
    requirementsContent = unlines
        [ "antlr4-python3-runtime>=4.13.0"
        , "antlr4-tools>=0.2.0"
        ]
    packageName = maybe id (+.+) inPackage $ mkName [] CamelCase lang
    langName = sanitizePythonModuleName $ mkName [] CamelCase lang

    importLangName = "import 'package:" ++ langName ++ "_generated/" ++ langName ++ "_generated.Python3';"

    lexerClassName = lang ++ "GrammarLexer"
    parserClassName = lang ++ "GrammarParser"

    makeVars x = [MakeFile.mkVar n v | (n,v) <- x]
    makeRules x = [MakeFile.mkRule tar dep recipe  | (tar, dep, recipe) <- x]
    makefileVars = vcat $ makeVars
      [("LANG", langName)
      , ("LEXER_NAME", upperFirst langName ++ "Lexer")
      , ("PARSER_NAME", upperFirst langName ++ "Parser")
      , ("ANTLR4", "antlr4")
      ]
    refVarInSrc dirBase refVar = dirBase </> MakeFile.refVar refVar
    rmFile :: (String -> String) -> String -> String -> String
    rmFile refSrcVar refVar ext = "rm -f" +++ refSrcVar refVar ++ ext
    makefileRules refSrcVar = 
      let rmInSrc = rmFile refSrcVar
      in vcat $ makeRules
        [ (".PHONY", ["all", "clean", "remove", "install"], [])
        , ("all", ["install", MakeFile.refVar "LANG"], [])
        , ("install", [],
            [ "pip install -r requirements.txt"
            ])
        , ("lexer"
            , ["install", refSrcVar "LEXER_NAME" ++ ".g4"]
            , [MakeFile.refVar "ANTLR4" +++ "-Dlanguage=Python3" +++ refSrcVar "LEXER_NAME" ++ ".g4"])
        , ("parser"
            , ["install", refSrcVar "PARSER_NAME" ++ ".g4"]
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

-- | Sanitize module name for Python (replace dashes with underscores, etc.)
sanitizePythonModuleName :: String -> String
sanitizePythonModuleName = map (\c -> if c == '-' then '_' else c)