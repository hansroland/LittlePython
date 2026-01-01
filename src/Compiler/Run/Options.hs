module Compiler.Run.Options (Settings(..), getOptions, 
    dumpUsage, options, testSettings) where  

import SimpleGetOpt 

data Settings = Settings
  { printInp :: Bool
  , printAst    :: Bool
  , printRco    :: Bool 
  , printSi     :: Bool
  , printAh     :: Bool
  , printPatch  :: Bool
  , printEpilog :: Bool
  , file        :: String
  , outdir      :: String }  
  deriving (Show)

getOptions :: IO Settings 
getOptions =  getOpts defaultSettings options

defaultSettings :: Settings
defaultSettings = Settings
  { printInp = False
  , printAst = False
  , printRco = False 
  , printSi  = False
  , printAh  = False
  , printPatch = False
  , printEpilog = False
  , outdir = "./bin"
  , file     = ""
  }

testSettings :: FilePath -> Settings
testSettings path =  defaultSettings{file = path }

options :: OptSpec Settings
options = optSpec
  { progDescription = [ "A toy compiler for the LittlePython language." ]

  , progOptions =
      [ Option ['i'] ["printInp"]
        "Display the source code."
        $ NoArg $ \s -> Right s { printInp = True }

      , Option ['a'] ["printAst"]
        "Display the AST."
        $ NoArg $ \s -> Right s { printAst = True }

      , Option ['c'] ["printRco"]
        "Display after the removal of complex operation."
        $ NoArg $ \s -> Right s { printRco = True }

      , Option ['s'] ["printSi"]
        "Display the first assembler code."
        $ NoArg $ \s -> Right s { printSi = True }

      , Option ['h'] ["printAh"]
        "Display after AssignHomes."
        $ NoArg $ \s -> Right s { printAh = True }

      , Option ['p'] ["printPatch"]
        "Display after Patch Instructions."
        $ NoArg $ \s -> Right s { printPatch = True }

      , Option ['e'] ["printEpilog"]
        "Display after adding Pro- and Epilog."
        $ NoArg $ \s -> Right s { printEpilog = True }

      , Option ['o'] ["outdir"]
        "Output directory for binaries"
        $ ReqArg "STRING" $ \a s -> 
             case null a of
              False -> Right $ s { outdir = a }
              True  -> Left "outdir is empty"
      ]
  , progParamDocs =
      [ ("FILE",   "The file that needs processing.") ]

  , progParams = \p s -> Right s { file = p }

  , progArgOrder = Permute
  }
